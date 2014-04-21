(define-library (os boot predicates)
  ;
  ;   Class relation predicates
  ;
  (import (scheme base)
          (os boot accessors) )

  (export subclass? nonstrict-subclass?)

  (begin

    (define (subclass? class superclass)
      (if (memq superclass (class-all-superclasses-ref class)) #t #f) )

    (define (nonstrict-subclass? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
