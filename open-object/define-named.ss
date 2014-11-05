#!r6rs
(library (open-object define-named)
  (export define-named)
  (import
    (rnrs)
    (open-object))

  (define-syntax define-named
    (syntax-rules ()
      ((define-named name expr)
        (define name
          (let ((value expr))
            (send value 'name= 'name)
            value)))
      ))
)
