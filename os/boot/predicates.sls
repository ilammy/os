#!r6rs
(library (os boot predicates)
  ;
  ;   Class relation predicates
  ;
  (export instance-of?
          subclass?
          nonstrict-subclass? )

  (import (except (rnrs base) assert)
          (rnrs lists)
          (os internal class-of)
          (os boot meta accessors) )

  (begin

    (define (instance-of? class object)
      (nonstrict-subclass? (class-of object) class) )

    (define (subclass? class superclass)
      (if (memq superclass (class-all-superclasses-ref class)) #t #f) )

    (define (nonstrict-subclass? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
