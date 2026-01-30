//! WASI TLS client host implementation.

use super::{
    CiphertextConsumer, CiphertextProducer, PlaintextConsumer, PlaintextProducer, mk_delete,
    mk_get_mut, mk_push,
};
use crate::p3::bindings::tls::client::{Connector, Host, HostConnector, HostConnectorWithStore};
use crate::p3::bindings::tls::types::Error;
use crate::p3::{TlsStream, TlsStreamClientArc, WasiTls, WasiTlsCtxView};
use core::pin::Pin;
use core::task::{Context, Poll};
use std::sync::{Arc, Mutex};
use tokio::sync::oneshot;
use wasmtime::component::{
    Access, Accessor, Destination, FutureReader, Resource, Source, StreamProducer, StreamReader,
    StreamResult,
};
use wasmtime::{StoreContextMut, component::StreamConsumer};

mk_push!(Error, push_error, "error");
mk_push!(Connector, push_connector, "client connector");
mk_get_mut!(Connector, get_connector_mut, "client connector");
mk_delete!(Connector, delete_connector, "client connector");

type PlaintextProducerClient = PlaintextProducer<rustls::ClientConnection>;

struct Pending<T> {
    inner_rx: oneshot::Receiver<T>,
    inner: Option<T>,
}

impl<T, D> StreamProducer<D> for Pending<T>
where
    T: StreamProducer<D> + Unpin,
{
    type Item = <T as StreamProducer<D>>::Item;
    type Buffer = <T as StreamProducer<D>>::Buffer;

    fn poll_produce<'a>(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        store: StoreContextMut<'a, D>,
        dst: Destination<'a, Self::Item, Self::Buffer>,
        finish: bool,
    ) -> Poll<wasmtime::Result<StreamResult>> {
        if let Some(ref mut inner) = self.inner {
            return Pin::new(inner).poll_produce(cx, store, dst, finish);
        }
        match Pin::new(&mut self.inner_rx).poll(cx) {
            Poll::Ready(Ok(inner)) => {
                self.inner = Some(inner);
                return self.poll_produce(cx, store, dst, finish);
            }
            Poll::Ready(Err(..)) => Poll::Ready(Ok(StreamResult::Dropped)),
            Poll::Pending if finish => Poll::Ready(Ok(StreamResult::Cancelled)),
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<T, D> StreamConsumer<D> for Pending<T>
where
    T: StreamConsumer<D> + Unpin,
{
    type Item = <T as StreamConsumer<D>>::Item;

    fn poll_consume(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        store: StoreContextMut<D>,
        src: Source<Self::Item>,
        finish: bool,
    ) -> Poll<wasmtime::Result<StreamResult>> {
        if let Some(ref mut inner) = self.inner {
            return Pin::new(inner).poll_consume(cx, store, src, finish);
        }
        match Pin::new(&mut self.inner_rx).poll(cx) {
            Poll::Ready(Ok(inner)) => {
                self.inner = Some(inner);
                return self.poll_consume(cx, store, src, finish);
            }
            Poll::Ready(Err(..)) => Poll::Ready(Ok(StreamResult::Dropped)),
            Poll::Pending if finish => Poll::Ready(Ok(StreamResult::Cancelled)),
            Poll::Pending => Poll::Pending,
        }
    }
}

struct SendProducer {
    rx: oneshot::Receiver<TlsStreamClientArc>,
    stream: Option<CiphertextProducer<rustls::ClientConnection>>,
}

impl<D> StreamProducer<D> for SendProducer
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
        if let Some(ref mut inner) = self.stream {
            return Pin::new(inner).poll_produce(cx, store, dst, finish);
        }
        match Pin::new(&mut self.rx).poll(cx) {
            Poll::Ready(Ok(stream)) => {
                self.stream = Some(CiphertextProducer(stream));
                Pin::new(self.stream.as_mut().unwrap()).poll_produce(cx, store, dst, finish)
            }
            Poll::Ready(Err(..)) => Poll::Ready(Ok(StreamResult::Dropped)),
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
        //let (tx, rx) = oneshot::channel();

        todo!()

        //let connector = get_connector_mut(store.get().table, &conn)?;
        //if connector.receive_tx.is_some() {
        //    return Err(wasmtime::Error::msg("send() already called"));
        //}
        //connector.cleartext_rx = Some(cleartext);
        //connector.receive_tx = Some(tx);

        //let mut ctx = store.as_context_mut();
        //let ciphertext = StreamReader::new(&mut ctx, SendProducer { rx, stream: None });
        //let result = FutureReader::new(&mut ctx, async { wasmtime::error::Ok(Ok(())) });
        //Ok((ciphertext, result))
    }

    fn receive<T>(
        mut store: Access<T, Self>,
        conn: Resource<Connector>,
        ciphertext: StreamReader<u8>,
    ) -> wasmtime::Result<(StreamReader<u8>, FutureReader<Result<(), Resource<Error>>>)>
    where
        T: 'static,
    {
        let (cons_tx, cons_rx) = oneshot::channel();
        let (prod_tx, prod_rx) = oneshot::channel();

        let conn @ Connector {
            receive_tx: None, ..
        } = get_connector_mut(store.get().table, &conn)?
        else {
            return Err(wasmtime::Error::msg("`receive` already called"));
        };
        conn.receive_tx = Some((prod_tx, cons_tx));

        let rx = StreamReader::new(
            &mut store,
            Pending {
                inner_rx: prod_rx,
                inner: None,
            },
        );
        ciphertext.pipe(
            &mut store,
            Pending {
                inner_rx: cons_rx,
                inner: None,
            },
        );
        Ok((
            rx,
            FutureReader::new(&mut store, async { wasmtime::error::Ok(Ok(())) }),
        ))
    }

    async fn connect<T>(
        store: &Accessor<T, Self>,
        conn: Resource<Connector>,
        server_name: String,
    ) -> wasmtime::Result<Result<(), Resource<Error>>>
    where
        T: 'static,
    {
        store.with(|mut store| {
            let server_name = match server_name.try_into() {
                Ok(name) => name,
                Err(err) => {
                    let err = push_error(store.get().table, format!("{err}"))?;
                    return Ok(Err(err));
                }
            };

            let connector = get_connector_mut(store.get().table, &conn)?;
            let cleartext_rx = connector
                .cleartext_rx
                .take()
                .ok_or_else(|| wasmtime::Error::msg("send() not called before connect()"))?;
            let ciphertext_rx = connector
                .ciphertext_rx
                .take()
                .ok_or_else(|| wasmtime::Error::msg("receive() not called before connect()"))?;
            let ciphertext_tx = connector.receive_tx.take().unwrap();
            let plaintext_tx = connector.send_tx.take().unwrap();

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

            let (error_tx, _error_rx) = oneshot::channel();
            let stream = Arc::new(Mutex::new(TlsStream::new(tls_conn, error_tx)));

            let _ = ciphertext_tx.send(Arc::clone(&stream));
            let _ = plaintext_tx.send(Arc::clone(&stream));

            cleartext_rx.pipe(
                &mut store,
                PlaintextConsumer::<_, rustls::client::ClientConnectionData>(Arc::clone(&stream)),
            );
            ciphertext_rx.pipe(&mut store, CiphertextConsumer(Arc::clone(&stream)));

            Ok(Ok(()))
        })
    }
}
