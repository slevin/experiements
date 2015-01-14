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

;; I could make it arraylike also by having a list pointer if the items are numeric?

;; there is the weak tables as well

;; setting a value to nil is a remove

;; setting also checks for set function first
;; and gettting checks for get function
;; and ltbl-call checks for that method in metatable?
;;  or can do

;;  __add and __mul, there are __sub (for subtraction), __div (for division), __unm (for negation), and __pow (for exponentiation). We may define also the field __concat, to define a behavior for the concatenation operator.

;; , through the metamethods __eq (equality), __lt (less than), and __le (less or equal). There are no separate metamethods for the other three relational operators, as Lua translates a ~= b to not (a == b), a > b to b < a, and a >= b to b <= a.

;;  __tostring field in the set metatable:
;; __metatable field in the metatable, getmetatable will return the value of this field, whereas setmetatable will raise an error:
;; I said earlier that, when we access an absent field in a table, the result is nil. This is true, but it is not the whole truth. Actually, such access triggers the interpreter to look for an __index metamethod: If there is no such method, as usually happens, then the access results in nil; otherwise, the metamethod will provide the result.

;; When Lua detects that w does not have the requested field, but has a metatable with an __index field, Lua calls this __index metamethod, with arguments w (the table) and "width" (the absent key). The metamethod then indexes the prototype with the given key and returns the result.
;; rawget and setting another ltbl as __index   http://www.lua.org/pil/13.4.1.html

;; __newindex and rawset(t, k, v)

;;I'm pretty sure there is a calling syntax like table()
(ltbl-setmeta tbl metatbl)
(ltbl-getmeta tbl)


(defun ltbl-new (&rest items)
  (list #s(hash-table data items) nil))

(defun ltbl-set (key value tbl)
  (if value
      (puthash key value (nth 0 tbl))
    (remhash key (nth 0 tbl))))

(defun ltbl-get (key tbl)
  (let ((val (gethash key (nth 0 tbl))))
    (unless val
      (ltbl-get '__index))))

;; if not there check metatable for __index method and call that


(defun ltbl-setmeta (tbl meta)
  (setcdr tbl (list meta)))

(defun ltbl-getmeta (tbl)
  (nth 1 tbl))
