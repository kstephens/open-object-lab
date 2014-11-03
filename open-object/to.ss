#!r6rs
;;; to syntax:
;;;
;;;   (to a (send b) (send c)) =>
;;;   (let ((v a))
;;;     (send v b)
;;;     (send v c)
;;;     v)
(library (open-object to)
  (export to)
  (import (rnrs))

  (define-syntax to
    (syntax-rules ()
      ((to expr subexpr subexprs ...)
        (let ((value expr))
          (to-1st value subexpr)
          (to-1st-exprs value subexprs) ...
          value))))

  (define-syntax to-1st-exprs
    (syntax-rules ()
      ((to-1st-exprs value)
        value)
      ((to-1st-exprs value app)
        (to-1st value app))
      ((to-1st-exprs value app apps ...)
        (begin (to-1st value app) (to-1st-exprs value apps ...)))))

  (define-syntax to-1st
    (syntax-rules ()
      ((to-1st value (proc args ...))
        (proc value args ...))))

)
