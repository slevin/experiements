#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-clang
  (ffi-lib "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib"))


(define-cstruct _CXString ([data _pointer]
                           [private_flags _ulong]))

(define-clang clang_getClangVersion (_fun -> _CXString))

(define-clang clang_getCString (_fun _CXString -> _bytes))

;; maybe have to dispose of cstring afterwards with dispose method
;; it does return a pointer that's probably alloced


(define-clang clang_createIndex (_fun _int _int -> _pointer))
(define _CXCursorKind _ulong) ;enum
(define _CXErrorCode _ulong) ;enum
(define _CXChildVisitResult _ulong) ;enum
(define-cstruct _CXCursor ([kind _CXCursorKind]
                           [xdata _int]
                           [data (make-array-type _pointer 3)]))

(define _CXTranslationUnit _pointer)

(define-clang clang_createTranslationUnit (_fun _pointer _string/utf-8 -> _pointer))

(define-clang clang_createTranslationUnit2 (_fun _pointer _string/utf-8 (o : (_ptr o _CXTranslationUnit))
                                                 -> (r : _CXErrorCode)
                                                 -> (values o r)))



(define-clang clang_createTranslationUnitFromSourceFile (_fun _pointer
                                                              _string/utf-8
                                                              _int
                                                              _pointer
                                                              _ulong
                                                              _pointer
                                                              -> _CXTranslationUnit))

(define idx (clang_createIndex 1 1))
;;(define file "/Users/slevin/wrk/tavi/tavi/WNDayPickerDayDataSource.m")
(define file "/Users/slevin/wrk/exp/racket/clang/simple.c")

;;(define-values (tu err) (clang_createTranslationUnit2 idx file))
;;(define tu (clang_createTranslationUnit idx file))
(define tu (clang_createTranslationUnitFromSourceFile idx
                                                      file
                                                      0
                                                      #f
                                                      0
                                                      #f))


(define-clang clang_getTranslationUnitCursor (_fun _CXTranslationUnit -> _CXCursor))
(define cu (clang_getTranslationUnitCursor tu))

(define visitCallback (_fun _CXCursor _CXCursor _pointer -> _CXChildVisitResult))
(define-clang clang_visitChildren (_fun _CXCursor visitCallback _pointer -> _ulong))

(define-clang clang_getCursorSpelling (_fun _CXCursor -> _CXString))
(define (visitor cursor parent data)
  (displayln (clang_getCString (clang_getCursorSpelling cursor)))
  (clang_visitChildren cursor visitor #f)
  1
  )

(clang_visitChildren cu visitor #f)

