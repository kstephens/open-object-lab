#!r6rs
;;; Piumarta and Warth's Open Objects in Scheme.
(library (open-object)
          (export <vtable> <object> set-vtable-proc! send object? object:_vt)
        (import (rnrs) (rnrs mutable-pairs (6)))

(define object:tag '(OBJECT))

(define (object:_vt  self)
  (vector-ref  self 1))
(define (object:_vt= self value)
  (vector-set! self 1 value))
 
(define (vtable:alloc self size)
  (let ((obj (make-vector (+ size 2))))
    (vector-set! obj 0 object:tag)
    (object:_vt= obj self)
    obj))
 
(define (vtable:parent  self)
  (vector-ref  self 2))
(define (vtable:parent= self value)
  (vector-set! self 2 value))
 
(define (vtable:methods  self)
  (vector-ref  self 3))
(define (vtable:methods= self value)
  (vector-set! self 3 value))

(define (vtable:with-parent self parent)
  (let ((child (vtable:alloc self 2)))
    (object:_vt=      child (and self (vtable self)))
    (vtable:parent=  child parent)
    (vtable:methods= child '())
    child))

(define (vtable:delegated self)
  (vtable:with-parent self #f))
 
(define <vtable> (vtable:delegated #f))
(define <object> (vtable:delegated #f))
 
(define (vtable:add-method self key value)
  (let* ( (methods (vtable:methods self))
          (slot (assq key methods)))
    (if slot
      (set-cdr! slot value)
      (vtable:methods= self (cons (cons key value) methods)))))
 
(define (vtable:lookup self key)
  (let* ((slot (assq key (vtable:methods self))))
    (if slot (cdr slot)
      (if (vtable:parent self)
        (send 'lookup (vtable:parent self) key)))))
 
(define (bind op rcvr)
  (let ((vt (vtable rcvr)))
    (if (and (eq? op 'lookup) (eq? vt <vtable>))
      (vtable:lookup vt op)
      (send 'lookup vt op))))
 
(define (send op self . args)
  (apply (bind op self) self args))
 
(define (object? self)
  (and (vector? self)
       (>= (vector-length self) 2)
       (eq? (vector-ref self 0) object:tag)))

(define vtable-proc
  (lambda (self)
    (cond
      ((object? self)   (object:_vt self))
      (else             <object>))))

(define (set-vtable-proc! proc)
  (set! vtable-proc proc))

(define (vtable self)
  (vtable-proc self))
 
;; Bootstrap vtables:
(begin
(object:_vt= <vtable> <vtable>)
(object:_vt= <object> <vtable>)
 
(vtable:parent= <vtable> <object>)
 
(vtable:add-method <vtable> 'lookup vtable:lookup)
(vtable:add-method <vtable> 'add-method vtable:add-method)
 
(send 'add-method <vtable> 'alloc vtable:alloc)
(send 'add-method <vtable> 'delegated vtable:delegated)
 
;; Additional vtable methods:
(send 'add-method <vtable> 'with-parent vtable:with-parent)

(send 'add-method <vtable> 'add-offset-accessor
      (lambda (self name offset)
        (set! offset (+ offset 2))
        (send 'add-method self name
              (lambda (self)       (vector-ref  self offset)))
        (send 'add-method self (string->symbol (string-append (symbol->string name) "="))
              (lambda (self value) (vector-set! self offset value)))))
)
)
