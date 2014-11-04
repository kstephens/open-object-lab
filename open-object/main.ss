#!r6rs
;;; Piumarta and Warth's Open Objects in Scheme.
(library (open-object)
  (export
    <vtable> <object>
    send send-via
    set-vtable-proc! object? object:_vt
    set-send-trace!)
  (import
    (rnrs)
    (rnrs mutable-pairs (6))
    (open-object to))

(define object:tag '(OBJECT))

(define (object:_vt  self)    (object:_slot  self -1))
(define (object:_vt= self v)  (object:_slot= self -1 v))
 
(define (vtable:alloc self size)
  (let ((obj (make-vector (+ size 2) #f)))
    (vector-set! obj 0 object:tag)
    (object:_vt= obj self)
    obj))
 
(define (object? self)
  (and (vector? self)
       (>= (vector-length self) 2)
       (eq? (vector-ref self 0) object:tag)))

(define (object:_slot  self i)   (vector-ref  self (+ i 2)))
(define (object:_slot= self i v) (vector-set! self (+ i 2) v))

(define (vtable:parent  self)    (object:_slot  self 1))
(define (vtable:parent= self v)  (object:_slot= self 1 v))
 
(define (vtable:methods  self)   (object:_slot  self 2))
(define (vtable:methods= self v) (object:_slot= self 2 v))

(define (vtable:new-vtable self size vtable parent)
  (let ((obj (vtable:alloc self size)))
    (object:_vt=     obj vtable)
    (vtable:parent=  obj parent)
    (vtable:methods= obj '())
    obj))

(define (vtable:with-parent-size self parent size)
  (vtable:new-vtable self size (and self (vtable self)) parent))

(define (vtable:with-parent self parent)
  (vtable:with-parent-size self parent 4))

(define (vtable:delegated self)
  (vtable:with-parent self #f))
 
(define <vtable> (vtable:delegated #f))
(define <object> (vtable:delegated #f))
 
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
    (if slot (cdr slot)
      (if (vtable:parent self)
        (send (vtable:parent self) 'lookup key)
        #f))))

(define (lookup vt op)
  (if (and (eq? op 'lookup) (eq? vt <vtable>))
    (vtable:lookup vt op)
    (send vt 'lookup op)))

(define (method:apply self rcvr vt op args)
  (cond
    ((procedure? self)
      (apply self rcvr args))
    ((not self)
      (send `(method:apply: ERROR: cannot find method for ,op in ,vt) 'write) (newline)
      (error "method:apply" `(cannot find method for ,op in ,vt)))
    (else
      (send self 'apply rcvr vt op args))))

(define (send-via rcvr vt op . args)
  (if *send-trace*
    (begin
      (send-w/o-trace `(send-via ,rcvr ,vt ,op . ,args) 'write) (newline)))
  (method:apply (lookup vt op) rcvr vt op args))

(define (send rcvr op . args)
  (apply send-via rcvr (vtable rcvr) op args))

(define *send-trace* #f)
(define (set-send-trace! v) (set! *send-trace* v))
(define (send-w/o-trace rcvr op . args)
  (let ((save *send-trace*))
    (set! *send-trace* #f)
    (let ((result (apply send rcvr op args)))
      (set! *send-trace* save)
      result)))

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
(object:_slot= <vtable> -1 <vtable>)
(object:_slot= <object> -1 <vtable>)

(to <vtable>
  (object:_slot= 1 <object>)

  (vtable:add-method 'lookup vtable:lookup)
  (vtable:add-method 'add-method vtable:add-method)
 
  (send 'add-method 'alloc vtable:alloc)
  (send 'add-method 'delegated vtable:delegated))
 
;; Additional vtable methods:
(to <vtable>
  (send 'add-method 'new-vtable vtable:new-vtable)
  (send 'add-method 'with-parent vtable:with-parent)
  (send 'add-method 'with-parent-size vtable:with-parent-size)
  (send 'add-method 'add-offset-accessor
        (lambda (self name offset)
          (set! offset (+ offset 2))
          (send self 'add-method name
                (lambda (self)       (vector-ref  self offset)))
          (send self 'add-method (string->symbol (string-append (symbol->string name) "="))
                (lambda (self value) (vector-set! self offset value)))))
  (send 'add-offset-accessor 'name 0)
  (send 'add-offset-accessor 'parent 1)
  (send 'add-offset-accessor 'methods 2)
  (send 'name= 'vtable)
)

;; Additional object methods:
(to <object>
  (send 'name= 'object)
  (send 'add-method '_slot  object:_slot)
  (send 'add-method '_slot= object:_slot=)
  (send 'add-method '_vtable vtable)
  (send 'add-method '_send send)
  (send 'add-offset-accessor '_vt -1)
)

))
