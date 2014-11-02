#!r6rs
;; Additional vtables for Scheme types:
(library (open-object scheme-types)
  (export
    slotted
    <slotted-class-class> <slotted-class>
    <slotted-object-class> <slotted-object>
    slotted-demo)
  (import (open-object) (rnrs))

  (define <slotted-class-class>  (send 'new-vtable <vtable> 8 <vtable> <vtable>))
  (define <slotted-class>        (send 'new-vtable <vtable> 8 <slotted-class-class> <slotted-class-class>))
  (define <slotted-object-class> (send 'new-vtable <vtable> 8 <vtable> <slotted-class-class>))
  (define <slotted-object>       (send 'new-vtable <vtable> 8 <slotted-class> <object>))
  (define slotted                (send 'alloc <slotted-class-class> 0))

  (define (slotted-demo)
    (display "  :: slotted-demo ::")(newline)
    (let ((cls #f) (obj #f))
    ; (set-send-trace! #t)
      (set! cls (send 'new-class slotted 'cls <slotted-object> '(a b c)))
      (send 'write cls)(newline)
      (set! obj (send 'new cls))
      (send 'a= obj 1)
      (send 'c= obj 3)
      (send 'write obj)(newline)
      ))

  (begin
    ; (send 'parent= <slotted-class> <slotted-object>)

    (send 'name= <slotted-class-class> 'slotted-class-class)
    (send 'name= <slotted-class> 'slotted-class)
    (send 'name= <slotted-object> 'slotted-object)

    (send 'add-offset-accessor <slotted-class-class> 'slots 3)
    (send 'add-offset-accessor <slotted-class-class> 'slot-i-map 4)
    (send 'add-offset-accessor <slotted-class-class> 'slots-size 5)

    (send 'slots= <slotted-object> '())
    (send 'slot-i-map= <slotted-object> '())
    (send 'slots-size= <slotted-object> 0)

    (send 'add-method <slotted-class-class>
      'new-class
      (lambda (self name parent slots)
        (let ((cls (send 'alloc <slotted-class> 8)))
          (send 'initialize cls name parent slots))))

    (send 'add-method <slotted-class>
      'initialize
      (lambda (self name parent slots)
        (send 'parent= self parent)
        (send 'methods= self '())
        (send 'name= self name)
        (send 'slots= self slots)
        ;; (send 'write (send '_vt parent))(newline)
        (let ( (i (if parent (send 'slots-size parent) 0))
               (slot-i-map '()))
          (for-each
            (lambda (slot)
              (send 'add-offset-accessor self slot i)
              (set! slot-i-map (cons (cons slot i) slot-i-map))
              (set! i (+ i 1))) slots)
          (send 'slot-i-map= self (reverse slot-i-map))
          (send 'slots-size= self i))
        self))

    (send 'add-method <slotted-class>
      'new (lambda (self)
             (send 'alloc self (send 'slots-size self))))

    ;; Addtional methods:
    (send 'add-method <slotted-object>
      'write (lambda (self)
               (display "#<")
               (send 'write (send '_vt self))
               (for-each
                 (lambda (slot-i)
                   (display " ")
                   (send 'write (car slot-i))
                   (display ": ")
                   (send 'write (send '_slot self (cdr slot-i)))
                   ) (send 'slot-i-map (send '_vt self)))
               (display " >")
               ))
    ))
