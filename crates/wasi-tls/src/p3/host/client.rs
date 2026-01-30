//! WASI TLS client host implementation.

use super::{
    CiphertextConsumer, CiphertextProducer, PlaintextConsumer, PlaintextProducer, mk_delete,
    mk_get_mut, mk_push,
};
use crate::p3::bindings::tls::client::{Connector, Host, HostConnector, HostConnectorWithStore};
use crate::p3::bindings::tls::types::Error;
use crate::p3::{TlsStream, TlsStreamClientArc, WasiTls, WasiTlsCtxView};
use core::mem;
use core::pin::Pin;
use core::task::{Context, Poll};
use std::sync::{Arc, Mutex};
use tokio::sync::oneshot;
use wasmtime::AsContextMut as _;
use wasmtime::StoreContextMut;
use wasmtime::component::{
    Access, Accessor, Destination, FutureReader, Resource, StreamProducer, StreamReader,
    StreamResult,
};

mk_push!(Error, push_error, "error");

mk_push!(Connector, push_connector, "client connector");
mk_get_mut!(Connector, get_connector_mut, "client connector");
mk_delete!(Connector, delete_connector, "client connector");

type PlaintextProducerClient = PlaintextProducer<rustls::ClientConnection>;

struct ReceiveProducer {
    stream_rx: oneshot::Receiver<TlsStreamClientArc>,
    stream: Option<PlaintextProducer<rustls::ClientConnection>>,
}

impl<D> StreamProducer<D> for ReceiveProducer
where
    D: 'static,
{
    type Item = <PlaintextProducerClient as StreamProducer<D>>::Item;
    type Buffer = <PlaintextProducerClient as StreamProducer<D>>::Buffer;

    fn poll_produce<'a>(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        store: StoreContextMut<'a, D>,
        dst: Destination<'a, Self::Item, Self::Buffer>,
        finish: bool,
    ) -> Poll<wasmtime::Result<StreamResult>> {
        if let Some(ref mut stream) = self.stream {
            return Pin::new(stream).poll_produce(cx, store, dst, finish);
        }
        match Pin::new(&mut self.stream_rx).poll(cx) {
            Poll::Ready(Ok(stream)) => {
                self.stream = Some(PlaintextProducer(stream));
                return self.poll_produce(cx, store, dst, finish);
            }
            Poll::Ready(Err(..)) => Poll::Ready(Ok(StreamResult::Dropped)),
            Poll::Pending if finish => Poll::Ready(Ok(StreamResult::Cancelled)),
            Poll::Pending => Poll::Pending,
        }
    }
}

struct PendingCiphertextProducer {
    rx: oneshot::Receiver<TlsStreamClientArc>,
    inner: Option<CiphertextProducer<rustls::ClientConnection>>,
}

impl<D> StreamProducer<D> for PendingCiphertextProducer
where
    D: 'static,
{
    type Item = u8;
    type Buffer = Option<u8>;

    fn poll_produce<'a>(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        store: StoreContextMut<'a, D>,
        dst: Destination<'a, Self::Item, Self::Buffer>,
        finish: bool,
    ) -> Poll<wasmtime::Result<StreamResult>> {
        // If we already have the inner producer, delegate to it.
        if let Some(ref mut inner) = self.inner {
            return Pin::new(inner).poll_produce(cx, store, dst, finish);
        }

        // Try to receive the stream.
        match Pin::new(&mut self.rx).poll(cx) {
            Poll::Ready(Ok(stream)) => {
                self.inner = Some(CiphertextProducer(stream));
                // Now poll the inner producer.
                Pin::new(self.inner.as_mut().unwrap()).poll_produce(cx, store, dst, finish)
            }
            Poll::Ready(Err(_)) => Poll::Ready(Ok(StreamResult::Dropped)),
            Poll::Pending if finish => Poll::Ready(Ok(StreamResult::Cancelled)),
            Poll::Pending => Poll::Pending,
        }
    }
}

impl Host for WasiTlsCtxView<'_> {}

impl HostConnector for WasiTlsCtxView<'_> {
    fn new(&mut self) -> wasmtime::Result<Resource<Connector>> {
        push_connector(&mut self.table, Connector::default())
    }

    fn drop(&mut self, conn: Resource<Connector>) -> wasmtime::Result<()> {
        delete_connector(&mut self.table, conn)?;
        Ok(())
    }
}

impl HostConnectorWithStore for WasiTls {
    fn send<T>(
        mut store: Access<T, Self>,
        conn: Resource<Connector>,
        cleartext: StreamReader<u8>,
    ) -> wasmtime::Result<(StreamReader<u8>, FutureReader<Result<(), Resource<Error>>>)>
    where
        T: 'static,
    {
        // Create a channel for the ciphertext producer to receive the TLS stream.
        let (ciphertext_tx, ciphertext_rx) = oneshot::channel();

        {
            let connector = get_connector_mut(store.get().table, &conn)?;

            // Update connector state based on current state.
            let old_state = mem::replace(connector, Connector::Exhausted);
            *connector = match old_state {
                Connector::Init => Connector::SendConfigured {
                    cleartext_rx: cleartext,
                    ciphertext_tx,
                },
                Connector::ReceiveConfigured {
                    ciphertext_rx,
                    plaintext_tx,
                } => Connector::Ready {
                    cleartext_rx: cleartext,
                    ciphertext_tx,
                    ciphertext_rx,
                    plaintext_tx,
                },
                other => {
                    *connector = other;
                    return Err(wasmtime::Error::msg(
                        "send() called in invalid state (already called or connect in progress)",
                    ));
                }
            };
        }

        let mut store_ctx = store.as_context_mut();

        // Return a ciphertext stream that will produce data once connected.
        let ciphertext = StreamReader::new(
            &mut store_ctx,
            PendingCiphertextProducer {
                rx: ciphertext_rx,
                inner: None,
            },
        );

        // Result future always succeeds (errors come through error_rx in connect).
        let result = FutureReader::new(&mut store_ctx, async { wasmtime::error::Ok(Ok(())) });

        Ok((ciphertext, result))
    }

    fn receive<T>(
        mut store: Access<T, Self>,
        conn: Resource<Connector>,
        ciphertext: StreamReader<u8>,
    ) -> wasmtime::Result<(StreamReader<u8>, FutureReader<Result<(), Resource<Error>>>)>
    where
        T: 'static,
    {
        let (tx, rx) = oneshot::channel();

        {
            let connector = get_connector_mut(store.get().table, &conn)?;

            // Update connector state based on current state.
            let old_state = mem::replace(connector, Connector::Exhausted);
            *connector = match old_state {
                Connector::Init => Connector::ReceiveConfigured {
                    ciphertext_rx: ciphertext,
                    plaintext_tx: tx,
                },
                Connector::SendConfigured {
                    cleartext_rx,
                    ciphertext_tx,
                } => Connector::Ready {
                    cleartext_rx,
                    ciphertext_tx,
                    ciphertext_rx: ciphertext,
                    plaintext_tx: tx,
                },
                other => {
                    *connector = other;
                    return Err(wasmtime::Error::msg(
                        "receive() called in invalid state (already called or connect in progress)",
                    ));
                }
            };
        }

        let mut store_ctx = store.as_context_mut();

        // Return a plaintext stream that will produce data once connected.
        let plaintext = StreamReader::new(
            &mut store_ctx,
            ReceiveProducer {
                stream_rx: rx,
                stream: None,
            },
        );

        // Result future always succeeds (errors come through error_rx in connect).
        let result = FutureReader::new(&mut store_ctx, async { wasmtime::error::Ok(Ok(())) });

        Ok((plaintext, result))
    }

    async fn connect<T>(
        store: &Accessor<T, Self>,
        conn: Resource<Connector>,
        server_name: String,
    ) -> wasmtime::Result<Result<(), Resource<Error>>>
    where
        T: 'static,
    {
        // Extract state from connector and create TLS connection.
        store.with(|mut store| {
            let server_name = match server_name.try_into() {
                Ok(server_name) => server_name,
                Err(err) => {
                    let err = push_error(store.get().table, format!("{err}"))?;
                    return Ok(Err(err));
                }
            };

            let connector = get_connector_mut(store.get().table, &conn)?;

            // Extract state.
            let old_state = mem::replace(connector, Connector::Exhausted);
            let (cleartext_rx, ciphertext_tx, ciphertext_rx, plaintext_tx) = match old_state {
                Connector::Ready {
                    cleartext_rx,
                    ciphertext_tx,
                    ciphertext_rx,
                    plaintext_tx,
                } => (cleartext_rx, ciphertext_tx, ciphertext_rx, plaintext_tx),
                other => {
                    *connector = other;
                    return Err(wasmtime::Error::msg(
                        "connect() called before send() and receive() were set up",
                    ));
                }
            };

            // Build root certificate store from webpki roots.
            let roots = rustls::RootCertStore {
                roots: webpki_roots::TLS_SERVER_ROOTS.into(),
            };

            let config = rustls::ClientConfig::builder()
                .with_root_certificates(roots)
                .with_no_client_auth();

            let tls_conn = match rustls::ClientConnection::new(Arc::from(config), server_name) {
                Ok(conn) => conn,
                Err(err) => {
                    let err = push_error(store.get().table, format!("{err}"))?;
                    return Ok(Err(err));
                }
            };

            let (error_tx, error_rx) = oneshot::channel();
            let stream = Arc::new(Mutex::new(TlsStream::new(tls_conn, error_tx)));

            // Store connected state.
            *connector = Connector::Connected {
                stream: Arc::clone(&stream),
                error_rx,
            };

            // Send stream to the pending producers so they can start producing.
            let _ = ciphertext_tx.send(Arc::clone(&stream));
            let _ = plaintext_tx.send(Arc::clone(&stream));

            // Pipe cleartext input to the TLS writer (plaintext consumer).
            cleartext_rx.pipe(
                &mut store,
                PlaintextConsumer::<_, rustls::client::ClientConnectionData>(Arc::clone(&stream)),
            );

            // Pipe ciphertext input to the TLS reader (ciphertext consumer).
            ciphertext_rx.pipe(&mut store, CiphertextConsumer(Arc::clone(&stream)));

            // Handshake will happen as streams are processed.
            // The handshake is driven by reading/writing data on the streams.
            // Return success - any errors will be reported through the stream futures.
            Ok(Ok(()))
        })
    }
}
