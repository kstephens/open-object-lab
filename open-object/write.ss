#!r6rs
(library (open-object write)
  (export write-demo)
  (import
    (open-object)
    (open-object scheme-types)
    (rnrs))

(define (write-demo)
  (display "  :: write-demo ::") (newline)
  (send <vtable> 'write) (newline)
  (send <object> 'write) (newline)
  (send 'a-symbol 'write) (newline)
  (send 123 'write) (newline)
  (send 1234.56 'write) (newline)
  (send 1/23 'write) (newline)
  (send '(a cons) 'write) (newline)
  (newline)
  )

(begin
  (send <scheme> 'add-method 'write write)
  (send <object> 'add-method
    'write (lambda (self)
             (display "#< ")
             (send (send self 'vtable) 'write)
             (display " ... >")
             ))
  (send <vtable> 'add-method
    'write (lambda (self)
             (display "#<vtable ")
             (send (send self 'name) 'write)
             (if (and #f (not (eq? <vtable> self)))
               (begin
                 (display "\n  _vt: ")
                 (send (send self '_vt) 'write)
                 (display "\n  parent: ")
                 (send (send self 'parent) 'write)
                 ))
             (display ">")
             ))
  (send <vector> 'add-method
    'write (lambda (self)
             (display "#(")
             (let loop ((i 0))
               (if (< i (vector-length self))
                 (begin
                   (if (> i 0) (display " "))
                   (send (vector-ref self i) 'write)
                   (loop (+ i 1)))))
             (display ")")))
  (send <pair> 'add-method
    'write (lambda (self)
             (display "(")
             (send (car self) 'write)
             (let loop ((obj (cdr self)))
               (cond
                 ((pair? obj)
                   (display " ")
                   (send (car obj) 'write)
                   (loop (cdr obj)))
                 ((null? obj)
                   (display ")"))
                 (else 
                   (display " . ")
                   (send obj 'write)
                   (display ")"))))))
  ))
