#!r6rs
;; Additional vtables for Scheme types:
(library (open-object scheme-types)
  (export
    <scheme>
    <boolean>
    <number> <complex> <real> <rational> <integer>
    <symbol>
    <sequence> <string> <list> <pair> <null> <vector> <port>)
  (import (open-object) (rnrs))

(define <scheme>   (send <vtable> 'with-parent <object>))
(define <boolean>  (send <vtable> 'with-parent <scheme>))
(define <number>   (send <vtable> 'with-parent <scheme>))
(define <complex>  (send <vtable> 'with-parent <number>))
(define <real>     (send <vtable> 'with-parent <complex>))
(define <rational> (send <vtable> 'with-parent <real>))
(define <integer>  (send <vtable> 'with-parent <rational>))
(define <symbol>   (send <vtable> 'with-parent <scheme>))
(define <sequence> (send <vtable> 'with-parent <scheme>))
(define <string>   (send <vtable> 'with-parent <sequence>))
(define <list>     (send <vtable> 'with-parent <sequence>))
(define <pair>     (send <vtable> 'with-parent <list>))
(define <null>     (send <vtable> 'with-parent <list>))
(define <vector>   (send <vtable> 'with-parent <sequence>))
(define <port>     (send <vtable> 'with-parent <sequence>))

(begin
  (send <scheme> 'name= 'scheme)
  (send <number> 'name= 'number)
  (send <complex> 'name= 'complex)
  (send <real> 'name= 'real)

;; Extend vtable determination into Scheme types:
(set-vtable-proc! (lambda (self)
  (cond
    ((object?  self)  (object:_vt self))
    ((boolean? self)  <boolean>)
    ((string?  self)  <string>)
    ((symbol?  self)  <symbol>)
    ((pair?    self)  <pair>)
    ((null?    self)  <null>)
    ((integer? self)  <integer>)
    ((rational? self) <rational>)
    ((real?    self)  <real>)
    ((complex? self)  <complex>)
    ((number?  self)  <number>)
    ((vector?  self)  <vector>)
    ((port?    self)  <port>)
    (else             <scheme>))))

  (send <scheme> 'add-method
    'write-to write)

  (send <vector> 'add-method
    'write-to (lambda (self port)
             (display "#(" port)
             (let loop ((i 0))
               (if (< i (vector-length self))
                 (begin
                   (if (> i 0) (display " " port))
                   (send (vector-ref self i) 'write-to port)
                   (loop (+ i 1)))))
             (display ")" port)))

  (send <pair> 'add-method
    'write-to (lambda (self port)
             (display "(" port)
             (send (car self) 'write-to port)
             (let loop ((obj (cdr self)))
               (cond
                 ((pair? obj)
                   (display " " port)
                   (send (car obj) 'write-to port)
                   (loop (cdr obj)))
                 ((null? obj)
                   (display ")" port))
                 (else
                   (display " . " port)
                   (send obj 'write-to port)
                   (display ")" port))))))

))
