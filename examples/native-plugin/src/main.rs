use anyhow::{Context as _, bail};
use clap::Parser;
use core::ffi::c_void;
use libloading::{Library, Symbol, library_filename};
use std::path::{Path, PathBuf};
use wasmtime::component::types::{self, ComponentItem};
use wasmtime::component::{Component, Linker, LinkerInstance, ResourceTable};
use wasmtime::{Engine, Store};
use wasmtime_wasi::p2::bindings::sync::Command;
use wasmtime_wasi::{WasiCtx, WasiCtxView, WasiView};

#[derive(Parser)]
#[command(name = "native-plugin-runtime", version = env!("CARGO_PKG_VERSION"))]
#[command(about = "A Wasmtime embedding with an native plugin loaded at runtime")]
struct Args {
    /// Plugin to load, this can either be an absolute path or a name,
    /// which will be looked up.
    /// Users should set the library lookup path appropriately
    /// for the default to work.
    #[arg(short, long, default_value = "rust_plugin")]
    plugin: Box<str>,

    /// Path to the `wasi:cli/command` component to run
    #[arg(
        short,
        long,
        default_value = "target/wasm32-wasip2/debug/native-plugin.wasm"
    )]
    component: PathBuf,
}

#[derive(Default)]
struct Ctx {
    wasi: WasiCtx,
    table: ResourceTable,
}

impl WasiView for Ctx {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.table,
        }
    }
}

struct Plugin {
    lib: Library,
}

unsafe fn dlsym<'a, T>(lib: &'a Library, symbol: &str) -> anyhow::Result<Symbol<'a, T>> {
    unsafe { lib.get::<T>(symbol.as_bytes()) }
        .with_context(|| format!("failed to lookup `{symbol}`"))
}

impl Plugin {
    fn load(src: impl AsRef<Path>) -> anyhow::Result<Self> {
        let src = src.as_ref();
        let lib =
            if src.has_root() || src.extension().is_some() || src.parent() != Some(Path::new("")) {
                unsafe { Library::new(src) }
            } else {
                unsafe { Library::new(library_filename(src)) }
            }
            .context("failed to load dynamic library")?;
        Ok(Self { lib })
    }

    fn add_to_linker(
        &self,
        engine: &wasmtime::Engine,
        linker: &mut LinkerInstance<Ctx>,
        instance_name: &str,
        ty: &types::ComponentInstance,
    ) -> anyhow::Result<()> {
        let Plugin { lib } = self;
        for (name, ty) in ty.exports(engine) {
            match ty {
                ComponentItem::ComponentFunc(..) => {
                    let symbol = format!("{instance_name}#{name}");
                    let f =
                        unsafe { dlsym::<unsafe extern "C" fn(*mut c_void, usize)>(lib, &symbol) }?;
                    let f = *f;
                    unsafe {
                        linker.func_new_unchecked(name, move |_store, _ty, storage| {
                            // TODO: pass a "context" to the plugin
                            f(storage.as_mut_ptr() as *mut _, storage.len());
                            Ok(())
                        })
                    }
                    .with_context(|| format!("failed to define function `{name}`"))?;
                }
                ComponentItem::Resource(_ty) => bail!("resources not supported yet"),
                ComponentItem::CoreFunc(..)
                | ComponentItem::Module(..)
                | ComponentItem::Component(..)
                | ComponentItem::ComponentInstance(..)
                | ComponentItem::Type(_) => {}
            }
        }
        Ok(())
    }
}

fn main() -> anyhow::Result<()> {
    let Args { plugin, component } = Args::parse();
    let engine = Engine::default();
    let component =
        Component::from_file(&engine, &component).context("failed to compile component")?;

    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;

    let plugin = Plugin::load(&*plugin)?;
    for (name, ty) in component.component_type().imports(&engine) {
        let (ns, tail) = name.split_once(':').unwrap_or(("", name));
        let (pkg, tail) = tail.split_once('/').unwrap_or(("", tail));
        let (interface, ver) = tail.split_once('@').unwrap_or((tail, ""));
        match (ty, ns, pkg, interface, ver) {
            (
                ComponentItem::ComponentInstance(..),
                "wasi",
                "cli" | "io" | "clocks" | "filesystem",
                _,
                ver,
            ) if ver.starts_with("0.2") => {
                // This instance is already provided by `wasmtime-wasi`
                continue;
            }
            (ComponentItem::ComponentInstance(ty), ..) => {
                let mut linker = linker.instance(name)?;
                plugin.add_to_linker(&engine, &mut linker, name, &ty)?;
            }
            _ => {
                bail!("do not know what to do with `{name}` import");
            }
        };
    }

    let wasi = WasiCtx::builder().inherit_stdio().build();
    let mut store = Store::new(
        &engine,
        Ctx {
            wasi,
            ..Ctx::default()
        },
    );
    let command = Command::instantiate(&mut store, &component, &linker)?;
    if let Err(()) = command.wasi_cli_run().call_run(&mut store)? {
        std::process::exit(1)
    }
    Ok(())
}
