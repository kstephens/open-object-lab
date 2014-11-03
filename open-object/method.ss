#!r6rs
;;; method objects with super.
(library (open-object method)
  (export <method>)
  (import
    (rnrs)
    (open-object)
    (open-object scheme-types)
    (open-object slotted))

  (define <method>
    (send slotted 'new-class 'method <slotted-object> '(proc op impl)))

  (begin
    (send <method> 'add-method 'initialize
      (lambda (self proc)
        (send self 'proc= proc)
        self))

    (send <method> 'add-method 'apply
      (lambda (self rcvr vt op args)
        (apply (send self 'proc) self rcvr args)))

    (send <method> 'add-method 'method-added-to
      (lambda (self impl op)
        (send self 'impl= impl)
        (send self 'op= op)))

    (send <method> 'add-method 'super
      (lambda (self rcvr . args)
        (apply send-via rcvr (send (send self 'impl) 'parent) (send self 'op) args)))
    ))
