#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-clang
  (ffi-lib "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib"))


(define-cstruct _CXString ([data _pointer]
                           [private_flags _ulong]))

(define-clang 


;; define the cxstring struct

;; define the version function that returns the cxstring
;; define the function that turns cxstring into cstring

;; call it and see?
