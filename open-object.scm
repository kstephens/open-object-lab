#lang r5rs
;;; Piumarta and Warth's Open Objects in Scheme.
(define object-tag '(OBJECT))
(define <vtable>   #f)
(define <object>   #f)

(define (object-_vt  self)
  (vector-ref  self 1))
(define (object-_vt= self value)
  (vector-set! self 1 value))
 
(define (vtable-alloc self size)
  (let ((obj (make-vector (+ size 2))))
    (vector-set! obj 0 object-tag)
    (object-_vt= obj self)
    obj))
 
(define (object? self)
  (and (vector? self)
       (>= (vector-length self) 2)
       (eq? (vector-ref self 0) object-tag)))
 
(define (vtable self)
  (cond
    ((object? self)   (object-_vt self))
    (else             <object>)))
 
(define (vtable-parent  self)
  (vector-ref  self 2))
(define (vtable-parent= self value)
  (vector-set! self 2 value))
 
(define (vtable-methods  self)
  (vector-ref  self 3))
(define (vtable-methods= self value)
  (vector-set! self 3 value))

(define (vtable-with-parent self parent)
  (let ((child (vtable-alloc self 2)))
    (object-_vt=      child (and self (vtable self)))
    (vtable-parent=  child parent)
    (vtable-methods= child '())
    child))

(define (vtable-delegated self)
  (vtable-with-parent self #f))
 
(define (vtable-add-method self key value)
  (let* ( (methods (vtable-methods self))
          (slot (assq key methods)))
    (if slot
      (set-cdr! slot value)
      (vtable-methods= self (cons (cons key value) methods)))))
 
(define (vtable-lookup self key)
  (let* ((slot (assq key (vtable-methods self))))
    (if slot (cdr slot)
      (if (vtable-parent self)
        (send 'lookup (vtable-parent self) key)))))
 
(define (bind op rcvr)
  (let ((vt (vtable rcvr)))
    (if (and (eq? op 'lookup) (eq? vt <vtable>))
      (vtable-lookup vt op)
      (send 'lookup vt op))))
 
(define (send op self . args)
  (apply (bind op self) self args))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap vtables:
 
(set! <vtable> (vtable-delegated #f))
(object-_vt= <vtable> <vtable>)
 
(set! <object> (vtable-delegated #f))
(object-_vt= <object> <vtable>)
 
(vtable-parent= <vtable> <object>)
 
(vtable-add-method <vtable> 'lookup vtable-lookup)
(vtable-add-method <vtable> 'add-method vtable-add-method)
 
(send 'add-method <vtable> 'alloc vtable-alloc)
(send 'add-method <vtable> 'delegated vtable-delegated)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional vtables for Scheme types:

(send 'add-method <vtable> 'with-parent vtable-with-parent)

(define <number>  (send 'with-parent <vtable> <object>))
(define <real>    (send 'with-parent <vtable> <number>))
(define <integer> (send 'with-parent <vtable> <real>))
(define <symbol>  (send 'with-parent <vtable> <object>))
 
;; Extend vtable determination into Scheme types:
(set! vtable (lambda (self)
  (cond
    ((integer? self)  <integer>)
    ((real?    self)  <real>)
    ((number?  self)  <number>)
    ((symbol?  self)  <symbol>)
    ((object?  self)  (object-_vt self))
    (else             <object>))))
 
(send 'add-method <object>
      'print (lambda (self) (write `(object ,self)) (newline)))
(send 'add-method <vtable>
      'print (lambda (self) (write `(vtable ...)) (newline)))
(send 'add-method <number>
      'print (lambda (self) (write `(number ,self)) (newline)))
(send 'add-method <real>
      'print (lambda (self) (write `(real ,self)) (newline)))
(send 'add-method <integer>
      'print (lambda (self) (write `(integer ,self)) (newline)))
(send 'add-method <symbol>
      'print (lambda (self) (write `(symbol ,self)) (newline)))
 
(send 'print <vtable>)
(send 'print <object>)
(send 'print 'a-symbol)
(send 'print 123)
(send 'print 1234.56)
(send 'print 1/23)
(send 'print '(a cons))

(send 'add-method <vtable> 'add-offset-accessor
      (lambda (self name offset)
        (set! offset (+ offset 2))
        (send 'add-method self name
              (lambda (self)       (vector-ref  self offset)))
        (send 'add-method self (string->symbol (string-append (symbol->string name) "="))
              (lambda (self value) (vector-set! self offset value)))))

(send 'add-offset-accessor <object> '_vt -1)
(send 'add-offset-accessor <vtable> 'parent 0)
(send 'add-offset-accessor <vtable> 'methods 1)

(define <slotted-vtable> (send 'alloc <vtable> 4))
(send 'parent= <slotted-vtable> <vtable>)
(send 'methods= <slotted-vtable> '())

(send 'add-offset-accessor <slotted-vtable> 'slots 2)
(send 'add-offset-accessor <slotted-vtable> 'slot-i-map 3)

(send 'add-method <slotted-vtable>
      'new (lambda (self . slots)
             (let ((obj (send 'alloc self 4))
                   (i 0)
                   (slot-i-map '()))
               (for-each (lambda (slot)
                           (set! slot-i-map (cons (cons slot i) slot-i-map))
                           (set! i (+ i 1))) slots)
               (send 'slots=      obj (list->vector slots))
               (send 'slot-i-map= obj slot-i-map)
               obj)))

#;
(define <slotted-object> (send 'new <slotted-vtable>))
