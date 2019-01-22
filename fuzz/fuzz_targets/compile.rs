#![no_main]

#[macro_use]
extern crate libfuzzer_sys;
extern crate cranelift_codegen;
extern crate cranelift_native;
extern crate wasmparser;
extern crate wasmtime_environ;
extern crate wasmtime_jit;

use cranelift_codegen::settings;
use wasmparser::validate;
use wasmtime_jit::{CompiledModule, Compiler, NullResolver};

fuzz_target!(|data: &[u8]| {
    if !validate(data, None) {
        return;
    }
    let flag_builder = settings::builder();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|_| {
        panic!("host machine is not a supported target");
    });
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));
    let mut compiler = Compiler::new(isa);
    let mut resolver = NullResolver {};
    let _compiled = match CompiledModule::new(&mut compiler, data, &mut resolver) {
        Ok(x) => x,
        Err(_) => return,
    };
});
