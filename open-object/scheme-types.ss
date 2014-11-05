#!r6rs
;; Additional vtables for Scheme types:
(library (open-object scheme-types)
  (export
    <scheme>
    <boolean>
    <number> <complex> <real> <rational> <integer>
    <symbol>
    <sequence> <string> <list> <pair> <null> <vector> <port>)
  (import (open-object) (open-object define-named) (rnrs))

(define-named <scheme>   (send <vtable> 'new-vtable <object>))
(define-named <boolean>  (send <vtable> 'new-vtable <scheme>))
(define-named <number>   (send <vtable> 'new-vtable <scheme>))
(define-named <complex>  (send <vtable> 'new-vtable <number>))
(define-named <real>     (send <vtable> 'new-vtable <complex>))
(define-named <rational> (send <vtable> 'new-vtable <real>))
(define-named <integer>  (send <vtable> 'new-vtable <rational>))
(define-named <symbol>   (send <vtable> 'new-vtable <scheme>))
(define-named <sequence> (send <vtable> 'new-vtable <scheme>))
(define-named <string>   (send <vtable> 'new-vtable <sequence>))
(define-named <list>     (send <vtable> 'new-vtable <sequence>))
(define-named <pair>     (send <vtable> 'new-vtable <list>))
(define-named <null>     (send <vtable> 'new-vtable <list>))
(define-named <vector>   (send <vtable> 'new-vtable <sequence>))
(define-named <port>     (send <vtable> 'new-vtable <sequence>))

(begin
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

(send <sequence> 'add-method
  'map (lambda (self proc)
         (let ((acc '()))
           (send self 'for-each
             (lambda (e)
               (set! acc (cons (proc e) acc))))
           (reverse acc))))
(send <string> 'add-method
  'for-each (lambda (self proc)
          (string-for-each self proc)))
(send <list> 'add-method
  'for-each (lambda (self proc)
          (for-each self proc)))
(send <vector> 'add-method
  'for-each (lambda (self proc)
          (vector-for-each self proc)))

))
