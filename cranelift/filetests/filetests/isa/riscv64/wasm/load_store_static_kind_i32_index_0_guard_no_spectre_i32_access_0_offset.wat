;;! target = "riscv64"
;;!
;;! settings = ['enable_heap_access_spectre_mitigation=false']
;;!
;;! compile = true
;;!
;;! [globals.vmctx]
;;! type = "i64"
;;! vmctx = true
;;!
;;! [globals.heap_base]
;;! type = "i64"
;;! load = { base = "vmctx", offset = 0, readonly = true }
;;!
;;! # (no heap_bound global for static heaps)
;;!
;;! [[heaps]]
;;! base = "heap_base"
;;! min_size = 0x10000
;;! offset_guard_size = 0
;;! index_type = "i32"
;;! style = { kind = "static", bound = 0x10000000 }

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; !!! GENERATED BY 'make-load-store-tests.sh' DO NOT EDIT !!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(module
  (memory i32 1)

  (func (export "do_store") (param i32 i32)
    local.get 0
    local.get 1
    i32.store offset=0)

  (func (export "do_load") (param i32) (result i32)
    local.get 0
    i32.load offset=0))

;; function u0:0:
;; block0:
;;   slli a3,a0,32
;;   srli a4,a3,32
;;   lui a3,65536
;;   addi a5,a3,-4
;;   bgtu a4,a5,taken(label3),not_taken(label1)
;; block1:
;;   ld a5,0(a2)
;;   add a4,a5,a4
;;   sw a1,0(a4)
;;   j label2
;; block2:
;;   ret
;; block3:
;;   udf##trap_code=heap_oob
;;
;; function u0:1:
;; block0:
;;   slli a2,a0,32
;;   srli a4,a2,32
;;   lui a2,65536
;;   addi a5,a2,-4
;;   bgtu a4,a5,taken(label3),not_taken(label1)
;; block1:
;;   ld a5,0(a1)
;;   add a4,a5,a4
;;   lw a0,0(a4)
;;   j label2
;; block2:
;;   ret
;; block3:
;;   udf##trap_code=heap_oob
