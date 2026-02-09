use anyhow::{Context as _, Result, anyhow, bail};
use core::future::Future;
use futures::join;
use test_programs::p3::wasi::sockets::ip_name_lookup::resolve_addresses;
use test_programs::p3::wasi::sockets::types::{IpAddress, IpSocketAddress, TcpSocket};
use test_programs::p3::wasi::tls::client::Connector;
use test_programs::p3::wit_stream;

struct Component;

test_programs::p3::export!(Component);

const PORT: u16 = 443;

async fn test_tls_sample_application(domain: &str, ip: IpAddress) -> Result<()> {
    let request = format!(
        "GET / HTTP/1.1\r\nHost: {domain}\r\nUser-Agent: wasmtime-wasi-rust\r\nConnection: close\r\n\r\n"
    );

    let sock = TcpSocket::create(ip.family()).unwrap();
    sock.connect(IpSocketAddress::new(ip, PORT))
        .await
        .context("tcp connect failed")?;

    // Set up TCP streams
    let (sock_rx, sock_rx_fut) = sock.receive();

    // Create TLS connector
    let conn = Connector::new();

    // Set up TLS decryption: ciphertext from network -> plaintext for app
    let (tls_rx, tls_rx_err_fut) = conn.receive(sock_rx);

    // Set up TLS encryption: plaintext from app -> ciphertext for network
    let (mut req_tx, req_rx) = wit_stream::new();
    let (sock_tx, tls_tx_err_fut) = conn.send(req_rx);

    let sock_tx_fut = sock.send(sock_tx);

    // Run everything concurrently - handshake, data transfer, and TCP I/O
    // The connect() call sets up the TLS state machine, and the actual handshake
    // bytes flow through the streams as they're being processed
    let (
        connect_result,
        send_result,
        recv_result,
        sock_rx_result,
        sock_tx_result,
        tls_rx_err_result,
        tls_tx_err_result,
    ) = join!(
        async {
            // Perform TLS handshake setup
            Connector::connect(conn, domain.into())
                .await
                .map_err(|err| anyhow!(err.to_debug_string()).context("TLS handshake failed"))
        },
        async {
            // Send HTTP request
            let remaining = req_tx.write_all(request.into()).await;
            assert!(remaining.is_empty(), "failed to write all request data");
            // Close the plaintext stream to signal we're done sending
            // This will trigger close_notify on the TLS connection
            drop(req_tx);
            Ok::<_, anyhow::Error>(())
        },
        async {
            // Collect all bytes from the TLS plaintext stream
            let response_data = tls_rx.collect().await;

            if response_data.is_empty() {
                bail!("connection closed before receiving response")
            }
            let response = String::from_utf8(response_data)?;
            if response.contains("HTTP/1.1 200 OK")
                || response.contains("HTTP/1.1 301")
                || response.contains("HTTP/1.1 302")
            {
                Ok(())
            } else {
                bail!("server did not respond with expected status: {response}")
            }
        },
        async { sock_rx_fut.await.context("TCP receive failed") },
        async { sock_tx_fut.await.context("TCP send failed") },
        async {
            tls_rx_err_fut
                .await
                .map_err(|err| anyhow!(err.to_debug_string()))
                .context("TLS receive error")
        },
        async {
            tls_tx_err_fut
                .await
                .map_err(|err| anyhow!(err.to_debug_string()))
                .context("TLS send error")
        },
    );

    connect_result?;
    send_result?;
    recv_result?;
    sock_rx_result?;
    sock_tx_result?;
    tls_rx_err_result?;
    tls_tx_err_result?;

    Ok(())
}

/// This test sets up a TCP connection using one domain, and then attempts to
/// perform a TLS handshake using another unrelated domain. This should result
/// in a handshake error.
async fn test_tls_invalid_certificate(_domain: &str, ip: IpAddress) -> Result<()> {
    const BAD_DOMAIN: &str = "wrongdomain.localhost";

    let sock = TcpSocket::create(ip.family()).unwrap();
    sock.connect(IpSocketAddress::new(ip, PORT))
        .await
        .context("tcp connect failed")?;

    // Set up TCP streams
    let (sock_rx, sock_rx_fut) = sock.receive();

    // Create TLS connector
    let conn = Connector::new();

    // Set up TLS decryption
    let (_tls_rx, tls_rx_err_fut) = conn.receive(sock_rx);

    // Set up TLS encryption
    let (_req_tx, req_rx) = wit_stream::new();
    let (sock_tx, tls_tx_err_fut) = conn.send(req_rx);
    let sock_tx_fut = sock.send(sock_tx);

    // Run everything concurrently - the handshake should fail due to certificate mismatch
    let (connect_result, _sock_rx_result, _sock_tx_result, _tls_rx_err_result, _tls_tx_err_result) =
        join!(
            async {
                // Attempt TLS handshake with wrong domain - should fail
                Connector::connect(conn, BAD_DOMAIN.into()).await
            },
            async { sock_rx_fut.await },
            async { sock_tx_fut.await },
            async { tls_rx_err_fut.await },
            async { tls_tx_err_fut.await },
        );

    match connect_result {
        Err(err) => {
            let debug_string = err.to_debug_string();
            // We're expecting an error regarding certificates in some form or
            // another. When we add more TLS backends this naive check will
            // likely need to be revisited/expanded:
            if debug_string.contains("certificate")
                || debug_string.contains("HandshakeFailure")
                || debug_string.contains("InvalidServerName")
                || debug_string.contains("invalid")
            {
                return Ok(());
            }
            bail!("unexpected error: {debug_string}")
        }
        Ok(()) => bail!("expecting server name mismatch error"),
    }
}

async fn try_live_endpoints<'a, Fut>(test: impl Fn(&'a str, IpAddress) -> Fut)
where
    Fut: Future<Output = Result<()>> + 'a,
{
    // since this is testing remote endpoints to ensure system cert store works
    // the test uses a couple different endpoints to reduce the number of flakes
    const DOMAINS: &[&str] = &[
        "example.com",
        "api.github.com",
        "docs.wasmtime.dev",
        "bytecodealliance.org",
        "www.rust-lang.org",
    ];

    for &domain in DOMAINS {
        let result = (|| async {
            let ip = resolve_addresses(domain.into())
                .await?
                .first()
                .map(|a| a.to_owned())
                .ok_or_else(|| anyhow!("DNS lookup failed."))?;
            test(domain, ip).await
        })();

        match result.await {
            Ok(()) => return,
            Err(e) => {
                eprintln!("test for {domain} failed: {e:#}");
            }
        }
    }

    panic!("all tests failed");
}

impl test_programs::p3::exports::wasi::cli::run::Guest for Component {
    async fn run() -> Result<(), ()> {
        println!("sample app");
        try_live_endpoints(test_tls_sample_application).await;
        println!("invalid cert");
        try_live_endpoints(test_tls_invalid_certificate).await;
        Ok(())
    }
}

fn main() {}
