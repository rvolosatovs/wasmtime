use core::ffi::c_char;

#[no_mangle]
pub extern "C" fn hello() -> *const c_char {
    c"hello".as_ptr()
}
