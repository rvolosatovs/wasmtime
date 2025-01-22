use core::future::Future;
use core::time::Duration;

use cap_std::time::SystemTime;
use tokio::time::sleep;
use wasmtime::{component, StoreContextMut};

use crate::p3::bindings::{
    clocks::monotonic_clock::{self, Duration as WasiDuration, Instant},
    clocks::wall_clock::{self, Datetime},
};
use crate::{WasiImpl, WasiView as _};

mod sync;

impl TryFrom<SystemTime> for Datetime {
    type Error = wasmtime::Error;

    fn try_from(time: SystemTime) -> Result<Self, Self::Error> {
        let duration =
            time.duration_since(SystemTime::from_std(std::time::SystemTime::UNIX_EPOCH))?;

        Ok(Self {
            seconds: duration.as_secs(),
            nanoseconds: duration.subsec_nanos(),
        })
    }
}

impl<T> wall_clock::Host for WasiImpl<T>
where
    T: crate::WasiView,
{
    fn now(&mut self) -> wasmtime::Result<Datetime> {
        let now = self.ctx().wall_clock.now();
        Ok(Datetime {
            seconds: now.as_secs(),
            nanoseconds: now.subsec_nanos(),
        })
    }

    fn resolution(&mut self) -> wasmtime::Result<Datetime> {
        let res = self.ctx().wall_clock.resolution();
        Ok(Datetime {
            seconds: res.as_secs(),
            nanoseconds: res.subsec_nanos(),
        })
    }
}

impl<T> monotonic_clock::Host for WasiImpl<T>
where
    T: crate::p3::WasiView,
    T::Data: crate::WasiView,
{
    type Data = T::Data;

    fn now(&mut self) -> wasmtime::Result<Instant> {
        Ok(self.ctx().monotonic_clock.now())
    }

    fn resolution(&mut self) -> wasmtime::Result<Instant> {
        Ok(self.ctx().monotonic_clock.resolution())
    }

    fn wait_until(
        mut store: StoreContextMut<'_, Self::Data>,
        when: Instant,
    ) -> impl Future<
        Output = impl FnOnce(StoreContextMut<'_, Self::Data>) -> wasmtime::Result<()> + 'static,
    > + 'static {
        let clock_now = store.data_mut().ctx().monotonic_clock.now();
        async move {
            if when > clock_now {
                sleep(Duration::from_nanos(when - clock_now)).await;
            };
            component::for_any(|_| Ok(()))
        }
    }

    fn wait_for(
        _store: StoreContextMut<'_, Self::Data>,
        duration: WasiDuration,
    ) -> impl Future<
        Output = impl FnOnce(StoreContextMut<'_, Self::Data>) -> wasmtime::Result<()> + 'static,
    > + 'static {
        async move {
            if duration > 0 {
                sleep(Duration::from_nanos(duration)).await;
            }
            component::for_any(|_| Ok(()))
        }
    }
}
