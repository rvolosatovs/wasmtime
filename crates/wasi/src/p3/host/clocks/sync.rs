use core::time::Duration;

use std::thread::sleep;

use crate::p3::bindings::clocks as async_clocks;
use crate::p3::bindings::sync::clocks as sync_clocks;
use crate::p3::bindings::sync::clocks::monotonic_clock::{Duration as WasiDuration, Instant};
use crate::{WasiImpl, WasiView as _};

impl<T> sync_clocks::monotonic_clock::Host for WasiImpl<T>
where
    T: crate::p3::WasiView,
    T::Data: crate::WasiView,
{
    fn now(&mut self) -> anyhow::Result<Instant> {
        async_clocks::monotonic_clock::Host::now(self)
    }

    fn resolution(&mut self) -> anyhow::Result<Instant> {
        async_clocks::monotonic_clock::Host::resolution(self)
    }

    fn wait_until(&mut self, when: Instant) -> anyhow::Result<()> {
        let clock_now = self.ctx().monotonic_clock.now();
        if when > clock_now {
            sleep(Duration::from_nanos(when - clock_now));
        }
        Ok(())
    }

    fn wait_for(&mut self, duration: WasiDuration) -> anyhow::Result<()> {
        if duration > 0 {
            sleep(Duration::from_nanos(duration));
        }
        Ok(())
    }
}
