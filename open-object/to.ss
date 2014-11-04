#!r6rs
;;; to syntax:
;;;
;;;   (to a (send b) (send c)) =>
;;;   (let ((v a))
;;;     (send v b)
;;;     (send v c)
;;;     v)
(library (open-object to)
  (export to to*)
  (import (rnrs))

  (define-syntax to*
    (syntax-rules ()
      ((to* (expr exprs ...) apps ...)
        (to expr
          (to* (exprs ...) apps ...)))
      ((to* (expr) apps ...)
        (to expr apps ...))
      ((to* () apps ...)
        (begin apps ...))
      ))

  (define-syntax to
    (syntax-rules ()
      ((to expr apps ...)
        (let ((value expr))
          (to-apps (value) apps ...)))))

  (define-syntax to-apps
    (syntax-rules ()
      ((to-apps (values ...))
        (begin values ...))
      ((to-apps (values ...) app apps ...)
        (begin
          (to-app (values ...) app)
          (to-apps (values ...) apps ...)))))

  (define-syntax to-app
    (syntax-rules (to)
      ((to-app (values ...) (to expr apps ...))
        (let ((value2 expr))
          (to-apps (values ... value2) apps ...)))
      ((to-app (values ...) (proc args ...))
        (proc values ... args ...))
      ))

)
