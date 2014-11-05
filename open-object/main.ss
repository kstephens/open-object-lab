#!r6rs
;;; Piumarta and Warth's Open Objects in Scheme.
(library (open-object)
  (export
    <vtable> <object>
    send
    set-vtable-proc! object? object:_vt)
  (import
    (rnrs)
    (rnrs mutable-pairs (6))
    (open-object to))

(define (send rcvr op . args)
  (apply (bind rcvr (vtable rcvr) op) rcvr args))
(define (send-vt rcvr vt op . args)
  (apply (bind rcvr vt op) rcvr args))
(define (bind rcvr vt op)
  (let ((meth (lookup vt op)))
    (cond
      ((procedure? meth) meth)
      ((not meth)
        (error "bind" `(no method for ,op in ,vt)))
      (else
        (send meth 'bind rcvr vt op)))))
(define (lookup vt op)
  (if (and (eq? op 'lookup) (eq? vt <vtable>))
    (vtable:lookup vt op)
    (send vt 'lookup op)))
(define (method:apply self rcvr vt op args)
  (cond
    ((procedure? self)
      (apply self rcvr args))
    ((not self)
      (error "method:apply" `(cannot find method for ,op in ,vt)))
    (else
      (send self 'apply rcvr vt op args))))

(define (object? self)
  (and (vector? self)
       (>= (vector-length self) 2)
       (eq? (vector-ref self 0) object:tag)))
(define (vtable:alloc self size)
  (let ((obj (make-vector (+ size 2) #f)))
    (object:_slot= obj -2 object:tag)
    (object:_slot= obj -1 self)
    obj))
(define object:tag '(OBJECT))
(define (object:_vt  self)    (object:_slot  self -1))
(define (object:_vt= self v)  (object:_slot= self -1 v))
(define (object:_slot  self i)    (vector-ref  self (+ i 2)))
(define (object:_slot= self i v)  (vector-set! self (+ i 2) v))

(define (vtable self)  (vtable-proc self))
(define vtable-proc (lambda (self)
    (if (object? self) (object:_slot self -1) <object>)))
(define (set-vtable-proc! proc)  (set! vtable-proc proc))

(define (vtable:new-vtable self parent . opts)
  (let* ((size (and (pair? opts) (car opts)))
         (obj (vtable:alloc self (or size 3))))
    (object:_slot= obj  1 parent)
    (object:_slot= obj  2 '())
    obj))
(define (vtable:methods  self)   (object:_slot  self 2))
(define (vtable:methods= self v) (object:_slot= self 2 v))
(define (vtable:add-method self key value)
  (let* ( (methods (vtable:methods self))
          (slot (assq key methods)))
    (if (not (procedure? value))
      (send value 'method-added-to self key))
    (if slot
      (set-cdr! slot value)
      (vtable:methods= self (cons (cons key value) methods)))))
(define (vtable:lookup self key)
  (let* ((slot (assq key (vtable:methods self))))
    (if slot
      (cdr slot)
      (let ((parent (object:_slot self 1)))
        (if parent
          (send parent 'lookup key)
          #f)))))

(define <vtable> (vtable:new-vtable #f #f))
(define <object> (vtable:new-vtable #f #f))

;; Bootstrap vtables:
(begin
(object:_slot= <vtable> -1 <vtable>)
(object:_slot= <object> -1 <vtable>)

(to <vtable>
  (object:_slot= 1 <object>)

  (vtable:add-method 'lookup vtable:lookup)
  (vtable:add-method 'add-method vtable:add-method)
 
  (send 'add-method 'alloc vtable:alloc)
  (send 'add-method 'new-vtable vtable:new-vtable))
 
;; Additional vtable methods:
(to <vtable>
  (to 'add-method
    (send 'add-offset-accessor
      (lambda (self name offset)
        (set! offset (+ offset 2))
        (send self 'add-method name
          (lambda (self)       (vector-ref  self offset)))
        (send self 'add-method (string->symbol (string-append (symbol->string name) "="))
          (lambda (self value) (vector-set! self offset value)))))
    )
  (to 'add-offset-accessor
    (send 'name 0)
    (send 'parent 1)
    (send 'methods 2))
  (send 'name= '<vtable>)
)

;; Additional object methods:
(to <object>
  (send 'name= '<object>)
  (to 'add-method
    (send '_slot  object:_slot)
    (send '_slot= object:_slot=)
    (send '_vtable vtable)
    (send '_send send)
    (send '_send-vt send-vt))
  (send 'add-offset-accessor '_vt -1)
)

))
