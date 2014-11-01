#!r6rs
;;; Piumarta and Warth's Open Objects in Scheme.
(library (open-object)
  (export
    <vtable> <object>
    send
    set-vtable-proc! object? object:_vt
    set-send-trace!)
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

(define (vtable:with-parent-size self parent size)
  (let ((child (vtable:alloc self size)))
    (object:_vt=      child (and self (vtable self)))
    (vtable:parent=  child parent)
    (vtable:methods= child '())
    child))

(define (vtable:with-parent self parent)
  (vtable:with-parent-size self parent 4))

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
        (send 'lookup (vtable:parent self) key)
        #f))))
 
(define (bind op rcvr)
  (let ((vt (vtable rcvr)))
    (if (and (eq? op 'lookup) (eq? vt <vtable>))
      (vtable:lookup vt op)
      (send 'lookup vt op))))

(define *send-trace* #f)
(define (set-send-trace! v)
  (set! *send-trace* v))

(define (_send op self . args)
  (let ((save *send-trace*))
    (set! *send-trace* #f)
    (let ((result (apply (bind op self) self args)))
      (set! *send-trace* save)
      result
      )))

(define (send op self . args)
  (if *send-trace*
    (begin
      (_send 'write `(send ,op ,self . ,args))(newline)))
  (let ((impl (bind op self)))
    (if (not impl)
      (begin
        (_send 'write `(send: cannot find ',op in ,(vtable self))) (newline)))
    (apply impl self args)))
 
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
(send 'add-method <vtable> 'with-parent-size vtable:with-parent-size)

(send 'add-method <vtable> 'add-offset-accessor
      (lambda (self name offset)
        (set! offset (+ offset 2))
        (send 'add-method self name
              (lambda (self)       (vector-ref  self offset)))
        (send 'add-method self (string->symbol (string-append (symbol->string name) "="))
              (lambda (self value) (vector-set! self offset value)))))

(send 'add-method <object> 'vtable vtable)
(send 'add-offset-accessor <object> '_vt -1)
(send 'add-offset-accessor <vtable> 'parent 0)
(send 'add-offset-accessor <vtable> 'methods 1)

(send 'add-offset-accessor <vtable> 'name 2)
(send 'name= <vtable> 'vtable)
(send 'name= <object> 'object)

))
