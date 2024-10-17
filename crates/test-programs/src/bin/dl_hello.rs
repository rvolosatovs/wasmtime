use test_programs::wasi::dl::dll;
use test_programs::wasi::dl::ffi::{FfiType, IncomingValue, PrimitiveType};

fn main() {
    let suf = dll::suffix();
    let pre = dll::prefix();
    let lib = dll::Library::open(
        Some(&format!("./tests/hello/target/debug/{pre}hello{suf}")),
        dll::OpenFlags::NOW,
    )
    .expect("failed to open library");
    let sym = lib.get("hello").expect("failed to get `hello` symbol");
    let func = sym.get_function(vec![], Some(FfiType::Primitive(PrimitiveType::Pointer)));
    let res = func
        .call(vec![])
        .expect("failed to call `hello`")
        .expect("`hello` is missing a return value");
    let IncomingValue::Primitive(res) = res else {
        panic!("`hello` return value is not a primitive");
    };
    let res = res
        .get_alloc()
        .expect("failed to get `hello` return value pointer")
        .read_string()
        .expect("failed to read `hello` string");
    assert_eq!(res, "hello");
}
