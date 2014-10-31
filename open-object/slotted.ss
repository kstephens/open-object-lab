#lang r5rs
(load "open-object.scm")
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
