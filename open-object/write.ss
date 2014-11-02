#!r6rs
(library (open-object write)
  (export write-demo)
  (import
    (open-object)
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
  (send <object> 'add-method 'write
    (lambda (self . port)
      (send self 'write-to (if (null? port) (current-output-port) (car port)))))
 
  (send <object> 'add-method
    'write-to (lambda (self port)
             (display "#< " port)
             (send (send self 'vtable) 'write-to port)
             (display " ... >" port)
             ))
  (send <vtable> 'add-method
    'write-to (lambda (self port)
             (display "#<vtable " port)
             (send (send self 'name) 'write-to port)
             (if (and #f (not (eq? <vtable> self)))
               (begin
                 (display "\n  _vt: " port)
                 (send (send self '_vt) 'write-to port)
                 (display "\n  parent: " port)
                 (send (send self 'parent) 'write-to port)
                 ))
             (display ">" port)
             ))
  ))
