#!r6rs
;; Additional vtables for Scheme types:
(library (open-object scheme-types)
  (export
    <scheme>
    <boolean>
    <number> <complex> <real> <rational> <integer>
    <symbol>
    <sequence> <string> <list> <pair> <null> <vector>)
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

(begin
  (send <scheme> 'name= 'scheme)
  (send <number> 'name= 'number)
  (send <complex> 'name= 'complex)
  (send <real> 'name= 'real)

;; Extend vtable determination into Scheme types:
(set-vtable-proc! (lambda (self)
  (cond
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
    ((object?  self)  (object:_vt self))
    ((vector?  self)  <vector>)
    (else             <scheme>))))
))
