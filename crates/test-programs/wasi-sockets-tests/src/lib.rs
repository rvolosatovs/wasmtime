wit_bindgen::generate!("test-command-with-sockets" in "../../wasi/wit");

use wasi::io::poll;
use wasi::io::streams;
use wasi::sockets::{network, tcp, tcp_create_socket, udp, udp_create_socket};

pub fn write(output: &streams::OutputStream, mut bytes: &[u8]) -> Result<(), streams::StreamError> {
    let pollable = output.subscribe();

    while !bytes.is_empty() {
        poll::poll_list(&[&pollable]);

        let permit = output.check_write()?;

        let len = bytes.len().min(permit as usize);
        let (chunk, rest) = bytes.split_at(len);

        output.write(chunk)?;

        output.blocking_flush()?;

        bytes = rest;
    }
    Ok(())
}

pub fn example_body_tcp(net: tcp::Network, sock: tcp::TcpSocket, family: network::IpAddressFamily) {
    let first_message = b"Hello, world!";
    let second_message = b"Greetings, planet!";

    let sub = sock.subscribe();

    sock.set_listen_backlog_size(32).unwrap();

    sock.start_listen().unwrap();
    poll::poll_one(&sub);
    sock.finish_listen().unwrap();

    let addr = sock.local_address().unwrap();

    let client = tcp_create_socket::create_tcp_socket(family).unwrap();
    let client_sub = client.subscribe();

    client.start_connect(&net, addr).unwrap();
    poll::poll_one(&client_sub);
    let (client_input, client_output) = client.finish_connect().unwrap();

    write(&client_output, &[]).unwrap();

    write(&client_output, first_message).unwrap();

    drop(client_input);
    drop(client_output);
    drop(client_sub);
    drop(client);

    poll::poll_one(&sub);
    let (accepted, input, output) = sock.accept().unwrap();

    let empty_data = input.read(0).unwrap();
    assert!(empty_data.is_empty());

    let data = input.blocking_read(first_message.len() as u64).unwrap();

    drop(input);
    drop(output);
    drop(accepted);

    // Check that we sent and recieved our message!
    assert_eq!(data, first_message); // Not guaranteed to work but should work in practice.

    // Another client
    let client = tcp_create_socket::create_tcp_socket(family).unwrap();
    let client_sub = client.subscribe();

    client.start_connect(&net, addr).unwrap();
    poll::poll_one(&client_sub);
    let (client_input, client_output) = client.finish_connect().unwrap();

    write(&client_output, second_message).unwrap();

    drop(client_input);
    drop(client_output);
    drop(client_sub);
    drop(client);

    poll::poll_one(&sub);
    let (accepted, input, output) = sock.accept().unwrap();
    let data = input.blocking_read(second_message.len() as u64).unwrap();

    drop(input);
    drop(output);
    drop(accepted);

    // Check that we sent and recieved our message!
    assert_eq!(data, second_message); // Not guaranteed to work but should work in practice.
}

pub fn example_body_udp(net: udp::Network, sock: udp::UdpSocket, family: network::IpAddressFamily) {
    let first_message = b"Hello, world!";
    let second_message = b"Greetings, planet!";

    let sub = sock.subscribe();

    let addr = sock.local_address().unwrap();

    let client = udp_create_socket::create_udp_socket(family).unwrap();
    let client_sub = client.subscribe();

    client.start_connect(&net, addr).unwrap();
    poll::poll_one(&client_sub);
    client.finish_connect().unwrap();

    let _client_addr = client.local_address().unwrap();

    let n = client
        .send(&[
            udp::Datagram {
                data: vec![],
                remote_address: addr,
            },
            udp::Datagram {
                data: first_message.to_vec(),
                remote_address: addr,
            },
        ])
        .unwrap();
    assert_eq!(n, 2);

    drop(client_sub);
    drop(client);

    poll::poll_one(&sub);
    let datagrams = sock.receive(2).unwrap();
    let mut datagrams = datagrams.into_iter();
    let (first, second) = match (datagrams.next(), datagrams.next(), datagrams.next()) {
        (Some(first), Some(second), None) => (first, second),
        (Some(_first), None, None) => panic!("only one datagram received"),
        (None, None, None) => panic!("no datagrams received"),
        _ => panic!("invalid datagram sequence received"),
    };

    assert!(first.data.is_empty());

    // TODO: Verify the `remote_address`
    //assert_eq!(first.remote_address, client_addr);

    // Check that we sent and recieved our message!
    assert_eq!(second.data, first_message); // Not guaranteed to work but should work in practice.

    // TODO: Verify the `remote_address`
    //assert_eq!(second.remote_address, client_addr);

    // Another client
    let client = udp_create_socket::create_udp_socket(family).unwrap();
    let client_sub = client.subscribe();

    client.start_connect(&net, addr).unwrap();
    poll::poll_one(&client_sub);
    client.finish_connect().unwrap();

    let n = client
        .send(&[udp::Datagram {
            data: second_message.to_vec(),
            remote_address: addr,
        }])
        .unwrap();
    assert_eq!(n, 1);

    drop(client_sub);
    drop(client);

    poll::poll_one(&sub);
    let datagrams = sock.receive(2).unwrap();
    let mut datagrams = datagrams.into_iter();
    let first = match (datagrams.next(), datagrams.next()) {
        (Some(first), None) => first,
        (None, None) => panic!("no datagrams received"),
        _ => panic!("invalid datagram sequence received"),
    };

    // Check that we sent and recieved our message!
    assert_eq!(first.data, second_message); // Not guaranteed to work but should work in practice.
}
