#!r6rs
;;; with syntax.
(library (open-object with)
  (export with)
  (import (rnrs))

  (define-syntax with
    (syntax-rules ()
      ((with expr subexpr subexprs ...)
        (let ((value expr))
          (with-1st value subexpr)
          (with-1st-exprs value subexprs) ...
          value))))

  (define-syntax with-1st-exprs
    (syntax-rules ()
      ((with-1st-exprs value)
        value)
      ((with-1st-exprs value app)
        (with-1st value app))
      ((with-1st-exprs value app apps ...)
        (begin (with-1st value app) (with-1st-exprs value apps ...)))))

  (define-syntax with-1st
    (syntax-rules ()
      ((with-1st value (proc args ...))
        (proc value args ...))))

)
