#!r6rs
(library (open-object write)
  (export write-demo)
  (import
    (open-object)
    (open-object scheme-types)
    (rnrs))

(define (write-demo)
  (display "  :: write-demo ::") (newline)
  (send 'write <vtable>) (newline)
  (send 'write <object>) (newline)
  (send 'write 'a-symbol) (newline)
  (send 'write 123) (newline)
  (send 'write 1234.56) (newline)
  (send 'write 1/23) (newline)
  (send 'write '(a cons)) (newline)
  (newline)
  )

(begin
  (send 'add-method <scheme> 'write write)
  (send 'add-method <object>
    'write (lambda (self)
             (display "#< ")
             (send 'write (send 'vtable self))
             (display " ... >")
             ))
  (send 'add-method <vtable>
    'write (lambda (self)
             (display "#<vtable ")
             (send 'write (send 'name self))
             (display ">")
             ))
  (send 'add-method <vector>
    'write (lambda (self)
             (display "#(")
             (let loop ((i 0))
               (if (< i (vector-length self))
                 (begin
                   (if (> i 0) (display " "))
                   (send 'write (vector-ref self i))
                   (loop (+ i 1)))))
             (display ")")))
  (send 'add-method <pair>
    'write (lambda (self)
             (display "(")
             (let loop ((obj self))
               (cond
                 ((pair? obj)
                   (send 'write (car obj))
                   (display " ")
                   (loop (cdr obj)))
                 ((null? obj)
                   (display ")"))
                 (else 
                   (display ". ")
                   (send 'write obj)
                   (display ")"))))))
  ))
