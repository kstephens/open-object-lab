open-object-lab
===============

Piumarta and Warth's Open Objects in R6RS Scheme.

A Scheme implementation of the object model as described in their paper:

* http://piumarta.com/software/cola/objmodel2.pdf

Demo:
-----

    Install Racket.
    $ ./run demo.ss

Overview
========

open-object/main.ss
-------------------

The basic object model and semantics as described in the paper.
Objects at this level are tagged Scheme vectors.
Everything above this level is accomplished by (send rcvr op . args).

Send is defined as:

    (define (send rcvr op . args)
      (apply (bind rcvr (vtable rcvr) op) rcvr args))

There are a few additional low-level provisions for extension:

* (bind rcvr vt op)
Delegates to (send method 'bind rcvr vt op) for methods that are not
Scheme procedures to produce a bound procedure.
* (vtable:add-method ...)
Invokes (send method 'method-added-to vt op) for methods that are not Scheme procedures.
* (object:vtable value)
Returns a vtable given any Scheme value.
* (object? value)
Return true if value is a tagged object vector.
* (send obj '_slot offset)
Read slot in tagged object vector.
* (send obj '_slot= offset value)
Write slot in tagged object vector.
* (send vtable 'name)
vtable name for debugging purposes.

open-object/write.ss
--------------------

Recursive object writer modeled after the Scheme (write) procedure.

open-object/scheme-type.ss
--------------------------

A vtable hierarchy for standard Scheme types.
Extends object:vtable with a (cond) on the Scheme type predicates.

open-object/slotted.ss
----------------------

Basic class-oriented objects with enumerated slots.

open-object/method.ss
---------------------

Methods that implement super.
Reifies the message being sent using the context of the vtable implementing
the method.

Uses protocols:
* (send method 'bind rcvr vt op)
* (send method 'method-added-to vt op)
