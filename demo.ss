#!r6rs
(import
  (open-object)
  (open-object scheme-types)
  (open-object write)
  (open-object slotted)
  (open-object method)
  (rnrs))

(define (write-demo)
  (display "\n  :: write-demo ::\n")
  (send <vtable> 'write) (newline)
  (send <object> 'write) (newline)
  (send 'a-symbol 'write) (newline)
  (send 123 'write) (newline)
  (send 1234.56 'write) (newline)
  (send 1/23 'write) (newline)
  (send '(a cons) 'write) (newline)
  (send <slotted-class> 'write) (newline)
  )
(write-demo)

(define (slotted-demo)
  (display "\n  :: slotted-demo ::\n")
  (let ((cls #f) (obj #f))
  ; (set-send-trace! #t)
    (set! cls (send slotted 'new-class <slotted-object> '(a b c)))
    (send cls 'add-method 'initialize
      (lambda (self b)
        (send self 'b= b)
        self))
    (send cls 'write)(newline)
    (set! obj (send cls 'new 2))
    (send obj 'a= 1)
    (send obj 'c= 3)
    (send obj 'write)(newline)
    ))
(slotted-demo)

(define (method-demo)
  (display "\n  :: method-demo ::\n")

  (send <object> 'add-method 'overridden
    (send <method> 'new (lambda (msg self)
                          (write `(<object> ,self))
                          (newline))))

  (send <number> 'add-method 'overridden
    (send <method> 'new (lambda (msg self)
                          (write `(<number> ,self))
                          (newline)
                          (send msg 'super))))

  (send <integer> 'add-method 'overridden
    (send <method> 'new (lambda (msg self)
                          (write `(<integer> ,self))
                          (newline)
                          (send msg 'super))))
  (send "foo" 'overridden)
  (send 1     'overridden)
  (send 2.34  'overridden)
  )
(method-demo)
