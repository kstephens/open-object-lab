#!r6rs
(library (open-object write)
  (export)
  (import
    (open-object)
    (rnrs))

(begin
  (send <object> 'add-method 'write
    (lambda (self . port)
      (send self 'write-to (if (null? port) (current-output-port) (car port)))))
 
  (send <object> 'add-method
    'write-to (lambda (self port)
             (display "#< " port)
             (send (send self '_vtable) 'write-to port)
             (display " ... >" port)
             ))
  (send <vtable> 'add-method
    'write-to (lambda (self port)
             (display "#<" port)
             (send (send (send self '_vtable) 'name) 'write-to port)
             (display " " port)
             (send (send self 'name) 'write-to port)
             (display ">" port)
             ))
  ))
