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

The basic <object> and <vtable> object model as described in the paper.
Objects at this level are tagged Scheme vectors.
Everything above this level is accomplished by (send rcvr op . args).
However, there are a few additional low-level provisions for extension:

* (object:vtable value)
Returns a <vtable> given any Scheme value.
* (object? value)
Return true if value is a tagged Scheme vector.
* (method:apply ...)
Delegates to (send 'apply) for methods that are not Scheme procedures.
* (send obj '_slot offset)
* (send obj '_slot= offset value)
Accessors into tagged Object vectors.
* (send vtable 'name)
<vtable> names for debugging purposes.

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
Implemented with slotted objects.
Uses the (send 'apply) protocol in (method:apply).

