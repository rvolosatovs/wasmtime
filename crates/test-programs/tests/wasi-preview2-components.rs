#![cfg(feature = "test_programs")]
use anyhow::Result;
use tempfile::TempDir;
use wasmtime::{component::Linker, Config, Engine, Store};
use wasmtime_wasi::preview2::{
    command::{add_to_linker, Command},
    pipe::MemoryOutputPipe,
    DirPerms, FilePerms, IsATTY, Table, WasiCtx, WasiCtxBuilder, WasiView,
};

lazy_static::lazy_static! {
    static ref ENGINE: Engine = {
        let mut config = Config::new();
        config.wasm_backtrace_details(wasmtime::WasmBacktraceDetails::Enable);
        config.wasm_component_model(true);
        config.async_support(true);

        let engine = Engine::new(&config).unwrap();
        engine
    };
}
// uses ENGINE, creates a fn get_component(&str) -> Component
include!(concat!(env!("OUT_DIR"), "/wasi_tests_components.rs"));

pub fn prepare_workspace(exe_name: &str) -> Result<TempDir> {
    let prefix = format!("wasi_components_{}_", exe_name);
    let tempdir = tempfile::Builder::new().prefix(&prefix).tempdir()?;
    Ok(tempdir)
}

async fn run(name: &str, inherit_stdio: bool) -> Result<()> {
    let workspace = prepare_workspace(name)?;
    let stdout = MemoryOutputPipe::new(4096);
    let stderr = MemoryOutputPipe::new(4096);
    let r = {
        let mut linker = Linker::new(&ENGINE);
        add_to_linker(&mut linker)?;

        // Create our wasi context.
        // Additionally register any preopened directories if we have them.
        let mut builder = WasiCtxBuilder::new();

        if inherit_stdio {
            builder.inherit_stdio();
        } else {
            builder
                .stdout(stdout.clone(), IsATTY::No)
                .stderr(stderr.clone(), IsATTY::No);
        }
        builder.args(&[name, "."]);
        println!("preopen: {:?}", workspace);
        let preopen_dir =
            cap_std::fs::Dir::open_ambient_dir(workspace.path(), cap_std::ambient_authority())?;
        builder.preopened_dir(preopen_dir, DirPerms::all(), FilePerms::all(), ".");
        for (var, val) in test_programs::wasi_tests_environment() {
            builder.env(var, val);
        }

        let mut table = Table::new();
        let wasi = builder.build(&mut table)?;
        struct Ctx {
            wasi: WasiCtx,
            table: Table,
        }
        impl WasiView for Ctx {
            fn ctx(&self) -> &WasiCtx {
                &self.wasi
            }
            fn ctx_mut(&mut self) -> &mut WasiCtx {
                &mut self.wasi
            }
            fn table(&self) -> &Table {
                &self.table
            }
            fn table_mut(&mut self) -> &mut Table {
                &mut self.table
            }
        }

        let ctx = Ctx { wasi, table };
        let mut store = Store::new(&ENGINE, ctx);
        let (command, _instance) =
            Command::instantiate_async(&mut store, &get_component(name), &linker).await?;
        command
            .wasi_cli_run()
            .call_run(&mut store)
            .await?
            .map_err(|()| anyhow::anyhow!("run returned a failure"))?;
        Ok(())
    };

    r.map_err(move |trap: anyhow::Error| {
        let stdout = stdout.try_into_inner().expect("single ref to stdout");
        if !stdout.is_empty() {
            println!("guest stdout:\n{}\n===", String::from_utf8_lossy(&stdout));
        }
        let stderr = stderr.try_into_inner().expect("single ref to stderr");
        if !stderr.is_empty() {
            println!("guest stderr:\n{}\n===", String::from_utf8_lossy(&stderr));
        }
        trap.context(format!(
            "error while testing wasi-tests {} with cap-std-sync",
            name
        ))
    })?;
    Ok(())
}

// Below here is mechanical: there should be one test for every binary in
// wasi-tests. The only differences should be should_panic annotations for
// tests which fail.
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn big_random_buf() {
    run("big_random_buf", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn clock_time_get() {
    run("clock_time_get", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn close_preopen() {
    run("close_preopen", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn dangling_fd() {
    run("dangling_fd", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn dangling_symlink() {
    run("dangling_symlink", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn directory_seek() {
    run("directory_seek", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn dir_fd_op_failures() {
    run("dir_fd_op_failures", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn fd_advise() {
    run("fd_advise", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn fd_filestat_get() {
    run("fd_filestat_get", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn fd_filestat_set() {
    run("fd_filestat_set", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn fd_flags_set() {
    run("fd_flags_set", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn fd_readdir() {
    run("fd_readdir", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn file_allocate() {
    run("file_allocate", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn file_pread_pwrite() {
    run("file_pread_pwrite", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn file_seek_tell() {
    run("file_seek_tell", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn file_truncation() {
    run("file_truncation", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn file_unbuffered_write() {
    run("file_unbuffered_write", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
#[cfg_attr(windows, should_panic)]
async fn interesting_paths() {
    run("interesting_paths", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn regular_file_isatty() {
    run("regular_file_isatty", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn nofollow_errors() {
    run("nofollow_errors", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn overwrite_preopen() {
    run("overwrite_preopen", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_exists() {
    run("path_exists", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_filestat() {
    run("path_filestat", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_link() {
    run("path_link", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_open_create_existing() {
    run("path_open_create_existing", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_open_read_write() {
    run("path_open_read_write", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_open_dirfd_not_dir() {
    run("path_open_dirfd_not_dir", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_open_missing() {
    run("path_open_missing", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_open_nonblock() {
    run("path_open_nonblock", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_rename_dir_trailing_slashes() {
    run("path_rename_dir_trailing_slashes", false)
        .await
        .unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
#[should_panic]
async fn path_rename_file_trailing_slashes() {
    run("path_rename_file_trailing_slashes", false)
        .await
        .unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_rename() {
    run("path_rename", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_symlink_trailing_slashes() {
    run("path_symlink_trailing_slashes", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn poll_oneoff_files() {
    run("poll_oneoff_files", false).await.unwrap()
}

#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn poll_oneoff_stdio() {
    run("poll_oneoff_stdio", true).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn readlink() {
    run("readlink", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
#[should_panic]
async fn remove_directory_trailing_slashes() {
    run("remove_directory_trailing_slashes", false)
        .await
        .unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn remove_nonempty_directory() {
    run("remove_nonempty_directory", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn renumber() {
    run("renumber", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn sched_yield() {
    run("sched_yield", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn stdio() {
    run("stdio", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn stdio_isatty() {
    // If the test process is setup such that stdio is a terminal:
    if test_programs::stdio_is_terminal() {
        // Inherit stdio, test asserts each is not tty:
        run("stdio_isatty", true).await.unwrap()
    }
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn stdio_not_isatty() {
    // Don't inherit stdio, test asserts each is not tty:
    run("stdio_not_isatty", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn symlink_create() {
    run("symlink_create", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn symlink_filestat() {
    run("symlink_filestat", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn symlink_loop() {
    run("symlink_loop", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn unlink_file_trailing_slashes() {
    run("unlink_file_trailing_slashes", false).await.unwrap()
}
#[test_log::test(tokio::test(flavor = "multi_thread"))]
async fn path_open_preopen() {
    run("path_open_preopen", false).await.unwrap()
}
