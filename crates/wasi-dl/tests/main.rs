use std::path::PathBuf;

use anyhow::{anyhow, Context as _, Result};
use test_programs_artifacts::{foreach_dl, DL_HELLO_COMPONENT};
use wasmtime::{
    component::{Component, Linker, ResourceTable},
    Store,
};
use wasmtime_wasi::{add_to_linker_async, bindings::Command, WasiCtx, WasiCtxBuilder, WasiView};
use wasmtime_wasi_dl::WasiDl;

struct Ctx {
    table: ResourceTable,
    wasi_ctx: WasiCtx,
}

impl WasiView for Ctx {
    fn table(&mut self) -> &mut ResourceTable {
        &mut self.table
    }

    fn ctx(&mut self) -> &mut WasiCtx {
        &mut self.wasi_ctx
    }
}

async fn run_wasi(path: &str, ctx: Ctx) -> Result<()> {
    let engine = test_programs_artifacts::engine(|config| {
        config.async_support(true);
    });
    let mut store = Store::new(&engine, ctx);
    let component = Component::from_file(&engine, path)?;

    let mut linker = Linker::new(&engine);
    add_to_linker_async(&mut linker)?;
    wasmtime_wasi_dl::add_to_linker(&mut linker, |h: &mut Ctx| WasiDl::new(&mut h.table))?;

    let command = Command::instantiate_async(&mut store, &component, &linker).await?;
    command
        .wasi_cli_run()
        .call_run(&mut store)
        .await?
        .map_err(|()| anyhow!("command returned with failing exit status"))
}

macro_rules! assert_test_exists {
    ($name:ident) => {
        #[allow(unused_imports)]
        use self::$name as _;
    };
}

foreach_dl!(assert_test_exists);

#[tokio::test(flavor = "multi_thread")]
async fn dl_hello() -> Result<()> {
    let res = tokio::process::Command::new(env!("CARGO"))
        .args(["build", "--manifest-path"])
        .arg(PathBuf::from_iter([
            env!("CARGO_MANIFEST_DIR"),
            "tests",
            "hello",
            "Cargo.toml",
        ]))
        .status()
        .await
        .context("failed to build `hello` library")?;
    assert!(res.success());

    run_wasi(
        DL_HELLO_COMPONENT,
        Ctx {
            table: ResourceTable::new(),
            wasi_ctx: WasiCtxBuilder::new().inherit_stdio().build(),
        },
    )
    .await
}
