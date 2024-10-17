//! # Wasmtime's [wasi-dl] Implementation
//!
//! This crate provides a Wasmtime host implementation of the [wasi-dl]
//! API. With this crate, the runtime can run components that call APIs in
//! [wasi-dl] and provide configuration variables for the component.
//!
//! # Examples
//!
//! The usage of this crate is very similar to other WASI API implementations
//! such as [wasi:cli] and [wasi:http].
//!
//! A common scenario is getting runtime-passed configurations in a [wasi:cli]
//! component. A standalone example of doing all this looks like:
//!
//! ```
//! use wasmtime::{
//!     component::{Linker, ResourceTable},
//!     Config, Engine, Result, Store,
//! };
//! use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiView};
//! use wasmtime_wasi_dl::WasiDl;
//!
//! #[tokio::main]
//! async fn main() -> Result<()> {
//!     let mut config = Config::new();
//!     config.async_support(true);
//!     let engine = Engine::new(&config)?;
//!
//!     let mut store = Store::new(&engine, Ctx {
//!         table: ResourceTable::new(),
//!         wasi_ctx: WasiCtxBuilder::new().build(),
//!     });
//!
//!     let mut linker = Linker::<Ctx>::new(&engine);
//!     wasmtime_wasi::add_to_linker_async(&mut linker)?;
//!     // add `wasi-dl` world's interfaces to the linker
//!     wasmtime_wasi_dl::add_to_linker(&mut linker, |h: &mut Ctx| {
//!         WasiDl::new(&mut h.table)
//!     })?;
//!
//!     // ... use `linker` to instantiate within `store` ...
//!
//!     Ok(())
//! }
//!
//! struct Ctx {
//!     table: ResourceTable,
//!     wasi_ctx: WasiCtx,
//! }
//!
//! impl WasiView for Ctx {
//!     fn table(&mut self) -> &mut ResourceTable { &mut self.table }
//!     fn ctx(&mut self) -> &mut WasiCtx { &mut self.wasi_ctx }
//! }
//! ```
//!
//! [wasi-dl]: https://github.com/WebAssembly/wasi-dl
//! [wasi:cli]: https://docs.rs/wasmtime-wasi/latest
//! [wasi:http]: https://docs.rs/wasmtime-wasi-http/latest

//#![deny(missing_docs)]
#![allow(unused)] // TODO: remove

use core::ffi::{
    c_double, c_float, c_int, c_long, c_longlong, c_schar, c_short, c_uchar, c_uint, c_ulong,
    c_ulonglong, c_ushort, c_void, CStr,
};
use core::mem::{transmute, zeroed};
use core::ptr::{addr_of_mut, null, null_mut};

use std::env::consts::{DLL_EXTENSION, DLL_PREFIX, DLL_SUFFIX};
use std::ffi::CString;

use anyhow::{bail, Context as _, Result};
use generated::wasi::dl::ffi::StructType;
use libffi_sys::{
    ffi_abi_FFI_DEFAULT_ABI, ffi_call, ffi_cif, ffi_prep_cif, ffi_status_FFI_BAD_ABI,
    ffi_status_FFI_BAD_ARGTYPE, ffi_status_FFI_BAD_TYPEDEF, ffi_status_FFI_OK, ffi_type,
    ffi_type_pointer, ffi_type_uint64, ffi_type_void,
};
use wasmtime::component::Resource;
use wasmtime_wasi::ResourceTable;

mod generated {
    wasmtime::component::bindgen!({
        path: "wit",
        world: "wasi:dl/imports",
        with: {
            "wasi:dl/dll/function": crate::Function,
            "wasi:dl/dll/library": crate::Library,
            "wasi:dl/dll/symbol": crate::Symbol,
            "wasi:dl/ffi/alloc": crate::Alloc,
            "wasi:dl/ffi/incoming-primitive": crate::IncomingPrimitive,
            "wasi:dl/ffi/incoming-struct": crate::IncomingStruct,
            "wasi:dl/ffi/outgoing-primitive": crate::OutgoingPrimitive,
            "wasi:dl/ffi/outgoing-struct": crate::OutgoingStruct,
        },
        trappable_imports: true,
    });
}
use self::generated::wasi::dl::dll::{self, OpenFlags};
use self::generated::wasi::dl::ffi::{self, FfiType, IncomingValue, OutgoingValue, PrimitiveType};

/// A wrapper capturing the needed internal `wasi-dl` state.
#[derive(Debug)]
pub struct WasiDl<'a> {
    table: &'a mut ResourceTable,
}

impl<'a> WasiDl<'a> {
    /// Creates a new instance of [WasiDl]
    pub fn new(table: &'a mut ResourceTable) -> Self {
        Self { table }
    }
}

/// Library opened via [`dlopen`](libc::dlopen)
pub struct Library(*mut c_void);

unsafe impl Send for Library {}

/// Symbol acquired via [`dlsym`](libc::dlsym)
pub struct Symbol(*mut c_void);

unsafe impl Send for Symbol {}

/// Function prepared using libffi
pub struct Function {
    cif: ffi_cif,
    func: unsafe extern "C" fn(),
    args: Box<[FfiType]>,
    ret: Option<FfiType>,
}

unsafe impl Send for Function {}

pub enum OutgoingPrimitive {
    CSchar(c_schar),
    CUchar(c_uchar),
    CShort(c_short),
    CUshort(c_ushort),
    CInt(c_int),
    CUint(c_uint),
    CLong(c_long),
    CUlong(c_ulong),
    CLongLong(c_longlong),
    CUlongLong(c_ulonglong),
    CFloat(c_float),
    CDouble(c_double),
    Int8T(i8),
    Int16T(i16),
    Int32T(i32),
    Int64T(i64),
    Uint8T(u8),
    Uint16T(u16),
    Uint32T(u32),
    Uint64T(u64),
    Pointer(*mut c_void),
}

unsafe impl Send for OutgoingPrimitive {}

pub enum IncomingPrimitive {
    CSchar(c_schar),
    CUchar(c_uchar),
    CShort(c_short),
    CUshort(c_ushort),
    CInt(c_int),
    CUint(c_uint),
    CLong(c_long),
    CUlong(c_ulong),
    CLongLong(c_longlong),
    CUlongLong(c_ulonglong),
    CFloat(c_float),
    CDouble(c_double),
    Int8T(i8),
    Int16T(i16),
    Int32T(i32),
    Int64T(i64),
    Uint8T(u8),
    Uint16T(u16),
    Uint32T(u32),
    Uint64T(u64),
    Pointer(*mut c_void),
}

unsafe impl Send for IncomingPrimitive {}

/// TODO: outgoing struct
pub struct OutgoingStruct;

unsafe impl Send for OutgoingStruct {}

/// TODO: incoming struct
pub struct IncomingStruct;

unsafe impl Send for IncomingStruct {}

pub enum Alloc {
    Local(*mut c_void),
    Foreign(*mut c_void),
}

unsafe impl Send for Alloc {}

impl From<PrimitiveType> for *mut ffi_type {
    fn from(ty: PrimitiveType) -> Self {
        match ty {
            PrimitiveType::CSchar => todo!(),
            PrimitiveType::CUchar => todo!(),
            PrimitiveType::CShort => todo!(),
            PrimitiveType::CUshort => todo!(),
            PrimitiveType::CInt => todo!(),
            PrimitiveType::CUint => todo!(),
            PrimitiveType::CLong => todo!(),
            PrimitiveType::CUlong => todo!(),
            PrimitiveType::CLongLong => todo!(),
            PrimitiveType::CUlongLong => todo!(),
            PrimitiveType::CFloat => todo!(),
            PrimitiveType::CDouble => todo!(),
            PrimitiveType::Int8T => todo!(),
            PrimitiveType::Int16T => todo!(),
            PrimitiveType::Int32T => todo!(),
            PrimitiveType::Int64T => todo!(),
            PrimitiveType::Uint8T => todo!(),
            PrimitiveType::Uint16T => todo!(),
            PrimitiveType::Uint32T => todo!(),
            PrimitiveType::Uint64T => unsafe { addr_of_mut!(ffi_type_uint64) },
            PrimitiveType::Pointer => unsafe { addr_of_mut!(ffi_type_pointer) },
        }
    }
}

impl ffi::Host for WasiDl<'_> {
    fn sizeof(&mut self, ty: PrimitiveType) -> wasmtime::Result<u8> {
        todo!()
    }
}

impl ffi::HostAlloc for WasiDl<'_> {
    fn new(&mut self, ty: FfiType) -> wasmtime::Result<Result<Resource<Alloc>, ()>> {
        todo!()
    }

    fn write_primitive(
        &mut self,
        alloc: Resource<Alloc>,
        v: Resource<OutgoingPrimitive>,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn write_struct(
        &mut self,
        alloc: Resource<Alloc>,
        v: Resource<OutgoingStruct>,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn write_string(
        &mut self,
        alloc: Resource<Alloc>,
        v: String,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn read_primitive(
        &mut self,
        alloc: Resource<Alloc>,
    ) -> wasmtime::Result<Result<Resource<IncomingPrimitive>, ()>> {
        todo!()
    }

    fn read_struct(
        &mut self,
        alloc: Resource<Alloc>,
    ) -> wasmtime::Result<Result<Resource<IncomingStruct>, ()>> {
        todo!()
    }

    fn read_string(&mut self, alloc: Resource<Alloc>) -> wasmtime::Result<Result<String, ()>> {
        let alloc = self
            .table
            .get(&alloc)
            .context("failed to get allocation resource from table")?;
        let ptr = match alloc {
            Alloc::Foreign(ptr) | Alloc::Local(ptr) => *ptr,
        };
        let s = unsafe { CStr::from_ptr(ptr.cast()) };
        let Ok(s) = s.to_str() else {
            return Ok(Err(()));
        };
        Ok(Ok(s.into()))
    }

    fn drop(&mut self, alloc: Resource<Alloc>) -> wasmtime::Result<()> {
        let alloc = self
            .table
            .delete(alloc)
            .context("failed to delete allocation resource from table")?;
        match alloc {
            Alloc::Foreign(..) => Ok(()),
            // TODO: free
            Alloc::Local(..) => Ok(()),
        }
    }
}

impl ffi::HostIncomingPrimitive for WasiDl<'_> {
    fn get_s8(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<i8, ()>> {
        todo!()
    }

    fn get_s16(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<i16, ()>> {
        todo!()
    }

    fn get_s32(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<i32, ()>> {
        todo!()
    }

    fn get_s64(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<i64, ()>> {
        todo!()
    }

    fn get_u8(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<u8, ()>> {
        todo!()
    }

    fn get_u16(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<u16, ()>> {
        todo!()
    }

    fn get_u32(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<u32, ()>> {
        todo!()
    }

    fn get_u64(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<u64, ()>> {
        todo!()
    }

    fn get_f32(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<f32, ()>> {
        todo!()
    }

    fn get_f64(&mut self, self_: Resource<IncomingPrimitive>) -> wasmtime::Result<Result<f64, ()>> {
        todo!()
    }

    fn get_alloc(
        &mut self,
        prim: Resource<IncomingPrimitive>,
    ) -> wasmtime::Result<Result<Resource<Alloc>, ()>> {
        let prim = self
            .table
            .get(&prim)
            .context("failed to get primitive resource from table")?;
        if let IncomingPrimitive::Pointer(ptr) = prim {
            let alloc = self
                .table
                .push(Alloc::Foreign(*ptr))
                .context("failed to push allocation to table")?;
            Ok(Ok(alloc))
        } else {
            Ok(Err(()))
        }
    }

    fn drop(&mut self, prim: Resource<IncomingPrimitive>) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl ffi::HostOutgoingPrimitive for WasiDl<'_> {
    fn new(&mut self, ty: PrimitiveType) -> wasmtime::Result<Resource<OutgoingPrimitive>> {
        todo!()
    }

    fn set_s8(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: i8,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_s16(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: i16,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_s32(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: i32,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_s64(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: i64,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_u8(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: u8,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_u16(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: u16,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_u32(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: u32,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_u64(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: u64,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_f32(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: f32,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_f64(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: f64,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn set_addr(
        &mut self,
        self_: Resource<OutgoingPrimitive>,
        v: Resource<Alloc>,
    ) -> wasmtime::Result<Result<(), ()>> {
        todo!()
    }

    fn drop(&mut self, rep: Resource<OutgoingPrimitive>) -> wasmtime::Result<()> {
        todo!()
    }
}

impl ffi::HostStructType for WasiDl<'_> {
    fn new(&mut self, fields: Vec<FfiType>) -> wasmtime::Result<Resource<StructType>> {
        todo!()
    }

    fn drop(&mut self, rep: Resource<StructType>) -> wasmtime::Result<()> {
        todo!()
    }
}

impl ffi::HostIncomingStruct for WasiDl<'_> {
    fn fields(&mut self, self_: Resource<IncomingStruct>) -> wasmtime::Result<Vec<IncomingValue>> {
        todo!()
    }

    fn drop(&mut self, rep: Resource<IncomingStruct>) -> wasmtime::Result<()> {
        todo!()
    }
}

impl ffi::HostOutgoingStruct for WasiDl<'_> {
    fn new(&mut self, fields: Vec<OutgoingValue>) -> wasmtime::Result<Resource<OutgoingStruct>> {
        todo!()
    }

    fn drop(&mut self, rep: Resource<OutgoingStruct>) -> wasmtime::Result<()> {
        todo!()
    }
}

impl dll::Host for WasiDl<'_> {
    fn extension(&mut self) -> wasmtime::Result<String> {
        Ok(DLL_EXTENSION.into())
    }

    fn prefix(&mut self) -> wasmtime::Result<String> {
        Ok(DLL_PREFIX.into())
    }

    fn suffix(&mut self) -> wasmtime::Result<String> {
        Ok(DLL_SUFFIX.into())
    }
}

impl dll::HostFunction for WasiDl<'_> {
    fn call(
        &mut self,
        func: Resource<Function>,
        args: Vec<OutgoingValue>,
    ) -> wasmtime::Result<Result<Option<IncomingValue>, ()>> {
        let func = self
            .table
            .get_mut(&func)
            .context("failed to get function resource from table")?;
        if !args.is_empty() {
            anyhow::bail!("arguments not currently supported");
        }
        let cif = addr_of_mut!(func.cif);
        let ptr = Some(func.func);
        let avalue = null_mut();
        let Some(ref ty) = func.ret else {
            unsafe { ffi_call(cif, ptr, null_mut(), avalue) }
            return Ok(Ok(None));
        };
        let ty = match ty {
            FfiType::Primitive(ty) => ty,
            FfiType::Struct(ty) => todo!(),
        };
        let prim = match ty {
            PrimitiveType::CSchar => {
                let mut rvalue: c_schar = 0;
                unsafe { ffi_call(cif, ptr, addr_of_mut!(rvalue).cast(), avalue) }
                IncomingPrimitive::CSchar(rvalue)
            }
            PrimitiveType::CUchar => todo!(),
            PrimitiveType::CShort => todo!(),
            PrimitiveType::CUshort => todo!(),
            PrimitiveType::CInt => todo!(),
            PrimitiveType::CUint => todo!(),
            PrimitiveType::CLong => todo!(),
            PrimitiveType::CUlong => todo!(),
            PrimitiveType::CLongLong => todo!(),
            PrimitiveType::CUlongLong => todo!(),
            PrimitiveType::CFloat => todo!(),
            PrimitiveType::CDouble => todo!(),
            PrimitiveType::Int8T => todo!(),
            PrimitiveType::Int16T => todo!(),
            PrimitiveType::Int32T => todo!(),
            PrimitiveType::Int64T => todo!(),
            PrimitiveType::Uint8T => todo!(),
            PrimitiveType::Uint16T => todo!(),
            PrimitiveType::Uint32T => todo!(),
            PrimitiveType::Uint64T => todo!(),
            PrimitiveType::Pointer => {
                let mut rvalue = null_mut::<c_void>();
                unsafe { ffi_call(cif, ptr, addr_of_mut!(rvalue).cast(), avalue) }
                IncomingPrimitive::Pointer(rvalue)
            }
        };
        let prim = self
            .table
            .push(prim)
            .context("failed to push primitive to table")?;
        Ok(Ok(Some(IncomingValue::Primitive(prim))))
    }

    fn from_alloc(
        &mut self,
        alloc: Resource<Alloc>,
        args: Vec<FfiType>,
        ret: Option<FfiType>,
    ) -> wasmtime::Result<Result<Resource<Function>, ()>> {
        todo!()
    }

    fn get_alloc(&mut self, func: Resource<Function>) -> wasmtime::Result<Resource<Alloc>> {
        todo!()
    }

    fn drop(&mut self, _func: Resource<Function>) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl dll::HostSymbol for WasiDl<'_> {
    fn get_function(
        &mut self,
        sym: Resource<Symbol>,
        args: Vec<FfiType>,
        ret: Option<FfiType>,
    ) -> wasmtime::Result<Resource<Function>> {
        let Symbol(sym) = self
            .table
            .get(&sym)
            .context("failed to get symbol resource from table")?;
        if !args.is_empty() {
            anyhow::bail!("arguments not currently supported");
        }
        let rtype = match ret {
            None => unsafe { addr_of_mut!(ffi_type_void) },
            Some(FfiType::Primitive(ty)) => ty.into(),
            Some(FfiType::Struct(ty)) => todo!(),
        };
        let atypes = null_mut();
        let mut cif = unsafe { zeroed() };
        let nargs = c_uint::try_from(args.len())
            .context("argument length does not fit in C `unsigned int`")?;
        let status = unsafe {
            ffi_prep_cif(
                addr_of_mut!(cif),
                ffi_abi_FFI_DEFAULT_ABI,
                nargs,
                rtype,
                atypes,
            )
        };
        if status == ffi_status_FFI_OK {
            let func = unsafe { transmute(*sym) };
            let args = args.into_boxed_slice();
            self.table
                .push(Function {
                    cif,
                    func,
                    args,
                    ret,
                })
                .context("failed to push function to table")
        } else if status == ffi_status_FFI_BAD_ABI {
            bail!("invalid `libffi` ABI")
        } else if status == ffi_status_FFI_BAD_TYPEDEF {
            bail!("invalid `libffi` type definition")
        } else {
            bail!("failed to initialize `libffi` CIF")
        }
    }

    fn get_alloc(
        &mut self,
        sym: Resource<Symbol>,
        ty: FfiType,
    ) -> wasmtime::Result<Resource<Alloc>> {
        todo!()
    }

    fn drop(&mut self, _sym: Resource<Symbol>) -> wasmtime::Result<()> {
        Ok(())
    }
}

impl dll::HostLibrary for WasiDl<'_> {
    fn open(
        &mut self,
        name: Option<String>,
        flag: OpenFlags,
    ) -> wasmtime::Result<Result<Resource<Library>, String>> {
        let mut cflag = 0;
        if flag.contains(OpenFlags::LAZY) {
            cflag |= libc::RTLD_LAZY
        }
        if flag.contains(OpenFlags::NOW) {
            cflag |= libc::RTLD_NOW
        }
        if flag.contains(OpenFlags::GLOBAL) {
            cflag |= libc::RTLD_GLOBAL
        }
        if flag.contains(OpenFlags::LOCAL) {
            cflag |= libc::RTLD_LOCAL
        }
        let lib = match name.map(CString::new) {
            None => unsafe { libc::dlopen(null(), cflag) },
            Some(Ok(name)) => unsafe { libc::dlopen(name.as_ptr(), cflag) },
            Some(Err(err)) => return Ok(Err(err.to_string())),
        };
        if lib.is_null() {
            let err = unsafe { libc::dlerror() };
            if !err.is_null() {
                let err = unsafe { CStr::from_ptr(err) };
                return Ok(Err(String::from_utf8_lossy(err.to_bytes()).into()));
            } else {
                return Ok(Err(String::from("`dlopen` returned NULL").into()));
            }
        }
        let lib = self
            .table
            .push(Library(lib))
            .context("failed to push library resource to table")?;
        Ok(Ok(lib))
    }

    fn get(
        &mut self,
        lib: Resource<Library>,
        name: String,
    ) -> wasmtime::Result<Result<Resource<Symbol>, String>> {
        let Library(lib) = self
            .table
            .get(&lib)
            .context("failed to get library resource from table")?;
        let name = match CString::new(name) {
            Ok(name) => name,
            Err(err) => return Ok(Err(err.to_string())),
        };
        let sym = unsafe { libc::dlsym(*lib, name.as_ptr()) };
        let err = unsafe { libc::dlerror() };
        if !err.is_null() {
            let err = unsafe { CStr::from_ptr(err) };
            return Ok(Err(String::from_utf8_lossy(err.to_bytes()).into()));
        }
        let sym = self
            .table
            .push(Symbol(sym))
            .context("failed to push symbol resource to table")?;
        Ok(Ok(sym))
    }

    fn drop(&mut self, lib: Resource<Library>) -> wasmtime::Result<()> {
        let Library(lib) = self
            .table
            .delete(lib)
            .context("failed to delete library resource from table")?;
        unsafe { libc::dlclose(lib) };
        Ok(())
    }
}

/// Add all the `wasi-dl` world's interfaces to a [`wasmtime::component::Linker`].
pub fn add_to_linker<T>(
    l: &mut wasmtime::component::Linker<T>,
    f: impl Fn(&mut T) -> WasiDl + Send + Sync + Copy + 'static,
) -> Result<()> {
    dll::add_to_linker_get_host(l, f)?;
    ffi::add_to_linker_get_host(l, f)?;
    Ok(())
}
