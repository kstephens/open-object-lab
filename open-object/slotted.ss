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

  (define (slotted-demo)
    (display "  :: slotted-demo ::")(newline)
    ;; (set-send-trace! #t)
    (let* (
            (cls (send 'new-class <slotted-metaclass> 'cls <slotted-object> '(a b c)))
            (obj (send 'new cls)))
      ;; (set-send-trace! #f)
      (send 'a= obj 1)
      (send 'c= obj 3)
      (send 'write obj)(newline)
      ))

  (begin
    (send 'name= <slotted-metaclass> 'slotted-metaclass)
    (send 'name= <slotted-class> 'slotted-class)
    (send 'name= <slotted-object> 'slotted-object)

    ;; (set-send-trace! #t)
    (send 'add-offset-accessor <slotted-object> '_slots 0)
    (send 'add-offset-accessor <slotted-metaclass> 'slots 4)
    (send 'add-offset-accessor <slotted-metaclass> 'slots-size 5)
    (send 'add-offset-accessor <slotted-metaclass> 'slot-i-map 6)

    (send 'add-method <vtable>
      'new-class (lambda (self name parent slots)
             (let ((cls (send 'alloc <slotted-metaclass> 7))
                    (i 0)
                    (slot-i-map '()))
               (send '_vt= cls <slotted-class>)
               (send 'parent= cls parent)
               (send 'methods= cls '())
               (send 'name= cls name)
               (send 'slots= cls slots)
               (for-each
                 (lambda (slot)
                   (send 'add-offset-accessor cls slot i)
                   (set! slot-i-map (cons (cons slot i) slot-i-map))
                   (set! i (+ i 1))) slots)
               (send 'slot-i-map= cls slot-i-map)
               (send 'slots-size= cls i)
               cls)))

    (send 'add-method <slotted-class>
      'new (lambda (self)
             (send 'alloc self (send 'slots-size self))))

    (send 'add-method <slotted-object>
      'write (lambda (self)
               (display "#<")
               (send 'write (send '_vt self))
               (for-each
                 (lambda (slot)
                   (display " ")
                   (send 'write (send slot self))
                   ) (send 'slots (send '_vt self)))
               (display ">")
               ))
    ))
