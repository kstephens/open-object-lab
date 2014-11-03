#!r6rs
;;; Slotted object classes.
(library (open-object scheme-types)
  (export
    slotted
    <slotted-class-class> <slotted-class>
    <slotted-object-class> <slotted-object>)
  (import
    (open-object)
    (open-object with)
    (rnrs))

  (define <slotted-class-class>  (send <vtable> 'new-vtable 8 <vtable> <vtable>))
  (define <slotted-class>        (send <vtable> 'new-vtable 8 <slotted-class-class> <slotted-class-class>))
  (define <slotted-object-class> (send <vtable> 'new-vtable 8 <vtable> <slotted-class-class>))
  (define <slotted-object>       (send <vtable> 'new-vtable 8 <slotted-class> <object>))
  (define slotted                (send <slotted-class-class> 'alloc 0))

  (begin
    (send <slotted-class-class> 'name= 'slotted-class-class)
    (send <slotted-class>  'name= 'slotted-class)
    (send <slotted-object> 'name= 'slotted-object)

    (with <slotted-class-class>
      (send 'add-offset-accessor 'slots 3)
      (send 'add-offset-accessor 'slot-i-map 4)
      (send 'add-offset-accessor 'slots-size 5))

    (with <slotted-object>
      (send 'slots= '())
      (send 'slot-i-map= '())
      (send 'slots-size= 0))

    (send <slotted-class-class> 'add-method
      'new-class
      (lambda (self name parent slots)
        (let ((cls (send <slotted-class> 'alloc 8)))
          (send cls 'initialize name parent slots))))

    (send <slotted-class> 'add-method
      'initialize
      (lambda (self name parent slots)
        (send self 'parent= parent)
        (send self 'methods= '())
        (send self 'name= name)
        (send self 'slots= slots)

        (let ( (i (if parent (send parent 'slots-size) 0))
               (slot-i-map '()))
          (for-each
            (lambda (slot)
              (send self 'add-offset-accessor slot i)
              (set! slot-i-map (cons (cons slot i) slot-i-map))
              (set! i (+ i 1))) slots)
          (send self 'slot-i-map= (reverse slot-i-map))
          (send self 'slots-size= i))
        self))

    (send <slotted-class> 'add-method
      'new (lambda (self . args)
             (apply send (send self 'alloc (send self 'slots-size)) 'initialize args)))

    (send <slotted-object> 'add-method
      'initialize (lambda (self) self))

    ;; Addtional methods:
    (send <slotted-object> 'add-method
      'write-to (lambda (self port)
               (display "#<" port)
               (send (send self '_vt) 'write-to port)
               (for-each
                 (lambda (slot-i)
                   (display " " port)
                   (send (car slot-i) 'write-to port)
                   (display ": " port)
                   (send (send self '_slot (cdr slot-i)) 'write-to port)
                   ) (send (send self '_vt) 'slot-i-map))
               (display " >" port)
               ))
    ))
