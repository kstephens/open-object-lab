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
