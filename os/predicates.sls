#!r6rs
(library (os predicates)
  ;
  ;   Class relation predicates
  ;
  (export instance-of?
          subclass?
          nonstrict-subclass? )

  (import (rnrs base)
          (os meta accessors)
          (os internal class-of) )

  (begin

    (define (instance-of? class object)
      (nonstrict-subclass? class (class-of object)) )

    (define (subclass? class superclass)
      (if (memq superclass (all-superclasses class)) #t #f) )

    (define (nonstrict-subclass? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
