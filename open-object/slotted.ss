#!r6rs
;; Additional vtables for Scheme types:
(library (open-object scheme-types)
  (export
    <slotted-metaclass> <slotted-object> <slotted-class>
    slotted-demo)
  (import (open-object) (rnrs))

  (define <slotted-metaclass> (send 'with-parent-size <vtable> <vtable> 8))
  (define <slotted-class>     (send 'with-parent-size <slotted-metaclass> <slotted-metaclass> 8))
  (define <slotted-object>    (send 'with-parent-size <slotted-class> <object> 8))
  (define <slotted>           (send 'alloc <slotted-metaclass> 8))

  (define (slotted-demo)
    (display "  :: slotted-demo ::")(newline)
    (let* (
            (cls (send 'new-class <slotted> 'cls <slotted-object> '(a b c)))
            (obj (send 'new cls)))
      (send 'a= obj 1)
      (send 'c= obj 3)
      (send 'write obj)(newline)
      ))

  (begin
    (send 'name= <slotted-metaclass> 'slotted-metaclass)
    (send 'name= <slotted-class> 'slotted-class)
    (send 'name= <slotted-object> 'slotted-object)

    (send 'add-offset-accessor <slotted-metaclass> 'slots 3)
    (send 'add-offset-accessor <slotted-metaclass> 'slots-size 4)
    (send 'add-offset-accessor <slotted-metaclass> 'slot-i-map 5)

    (send 'add-method <slotted-metaclass>
      'new-class (lambda (self name parent slots)
                   (send 'initialize (send 'alloc <slotted-class> 7)
                     name parent slots)))

    (send 'add-method <slotted-class>
      'initialize (lambda (self name parent slots)
             (let ( (i 0)
                    (slot-i-map '()))
               (send '_vt= self <slotted-class>)
               (send 'parent= self parent)
               (send 'methods= self '())
               (send 'name= self name)
               (send 'slots= self slots)
               (for-each
                 (lambda (slot)
                   (send 'add-offset-accessor self slot i)
                   (set! slot-i-map (cons (cons slot i) slot-i-map))
                   (set! i (+ i 1))) slots)
               (send 'slot-i-map= self (reverse slot-i-map))
               (send 'slots-size= self i)
               self)))

#|
    (send 'initialize <slotted-class>
      'slotted-class
      <slotted-object>
      '(parent methods name slots slots-size slots-i-map))
|#

    (send 'add-method <slotted-class>
      'new (lambda (self)
             (send 'alloc self (send 'slots-size self))))

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
