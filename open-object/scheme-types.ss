#!r6rs
;; Additional vtables for Scheme types:
(library (open-object scheme-types)
  (export
    <number> <complex> <real> <rational> <integer>
    <symbol>
    <sequence> <list> <pair> <null> <vector>)
  (import (open-object) (rnrs))

(define <number>  (send 'with-parent <vtable> <object>))
(define <complex> (send 'with-parent <vtable> <number>))
(define <real>    (send 'with-parent <vtable> <complex>))
(define <rational> (send 'with-parent <vtable> <real>))
(define <integer> (send 'with-parent <vtable> <real>))
(define <symbol>  (send 'with-parent <vtable> <object>))
(define <sequence> (send 'with-parent <vtable> <object>))
(define <list>     (send 'with-parent <vtable> <sequence>))
(define <pair>     (send 'with-parent <vtable> <list>))
(define <null>     (send 'with-parent <vtable> <list>))
(define <vector>   (send 'with-parent <vtable> <sequence>))

(begin
;; Extend vtable determination into Scheme types:
(set-vtable! (lambda (self)
  (cond
    ((integer? self)  <integer>)
    ((rational? self) <rational>)
    ((real?    self)  <real>)
    ((complex? self)  <complex>)
    ((number?  self)  <number>)
    ((symbol?  self)  <symbol>)
    ((object?  self)  (object:_vt self))
    ((pair?    self)  <pair>)
    ((null?    self)  <null>)
    ((vector?  self)  <vector>)
    (else             <object>))))
))
