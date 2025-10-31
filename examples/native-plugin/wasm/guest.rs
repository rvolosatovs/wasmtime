// Use wit_bindgen to generate the bindings from the component model to Rust.
// For more information see: https://github.com/bytecodealliance/wit-bindgen/
mod bindings {
    wit_bindgen::generate!({
        path: "..",
        world: "imports",
        generate_all,
    });
}

fn main() {
    bindings::docs::greet::greet::greet("wasmtime");
}
