;; what would a a lua style object system be like in elisp

;; table is a hash or maybe some other data structure
;; key/values
;; some literal syntax or constructor
;; if I do (call table method-key params)
;;   will look up that

;; has some __properties__ that can serve
;; as more rich methods
;; if __call__ is defined
;;   I that gets called with all params if not defined
;;
;; there's a get and a set metamethod
;; among others (that maybe I'd have to pass on)

;; could even redefine some basic emacs objects
;; in terms of these things so they are a bit extensible

;; don't need polymorphic dispatch because anything can be
;; anything from late binding
