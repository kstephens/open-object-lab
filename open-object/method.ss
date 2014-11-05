#!r6rs
;;; method objects with super.
(library (open-object method)
  (export <method>)
  (import
    (rnrs)
    (open-object)
    (open-object to)
    (open-object define-named)
    (open-object scheme-types)
    (open-object slotted))

  (define-named <method>
    (to (send slotted 'new-class <slotted-object> '(proc op impl))
      (to 'add-method
        (send 'initialize
          (lambda (self proc)
            (send self 'proc= proc)
            self))
        (send 'apply
          (lambda (self rcvr vt op args)
            (apply (send self 'proc) self rcvr args)))
        (send 'method-added-to
          (lambda (self impl op)
            (send self 'impl= impl)
            (send self 'op= op)))
        (send 'super
          (lambda (self rcvr . args)
            (apply send-via rcvr (send (send self 'impl) 'parent) (send self 'op) args)))
        )))
  )
