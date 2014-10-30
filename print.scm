#lang r5rs

(send 'add-method <object>
      'print (lambda (self) (write `(object ,self)) (newline)))
(send 'add-method <vtable>
      'print (lambda (self) (write `(vtable ...)) (newline)))
(send 'add-method <number>
      'print (lambda (self) (write `(number ,self)) (newline)))
(send 'add-method <real>
      'print (lambda (self) (write `(real ,self)) (newline)))
(send 'add-method <integer>
      'print (lambda (self) (write `(integer ,self)) (newline)))
(send 'add-method <symbol>
      'print (lambda (self) (write `(symbol ,self)) (newline)))
 
(send 'print <vtable>)
(send 'print <object>)
(send 'print 'a-symbol)
(send 'print 123)
(send 'print 1234.56)
(send 'print 1/23)
(send 'print '(a cons))

