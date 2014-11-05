#!r6rs
;;; method objects with super.
(library (open-object method)
  (export <method> <message>)
  (import
    (rnrs)
    (open-object)
    (open-object to)
    (open-object define-named)
    (open-object scheme-types)
    (open-object slotted))

  (define-named <method>
    (to (send slotted 'new-class <slotted-object> '(proc op vt))
      (to 'add-method
        (send 'initialize
          (lambda (self proc)
            (send self 'proc= proc)
            self))
        (send 'method-added-to
          (lambda (self vt op)
            (send self 'vt= vt)
            (send self 'op= op)))
        (send 'bind
          (lambda (self rcvr vt op)
            (let ((message (send <message> 'new rcvr (send self 'vt) op))
                   (proc (send self 'proc)))
              (lambda (rcvr . args)
                (apply proc message rcvr args)))))
        )))

  (define-named <message>
    (to (send slotted 'new-class <slotted-object> '(rcvr vt op))
      (to 'add-method
        (send 'initialize
          (lambda (self rcvr vt op)
            (send self 'rcvr= rcvr)
            (send self 'vt= vt)
            (send self 'op= op)
            self))
        (send 'super
          (lambda (self . args)
            (apply send (send self 'rcvr) '_send-vt (send (send self 'vt) 'parent) (send self 'op) args)))
        )))
  )
