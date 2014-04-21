(define-library (os predicates)
  ;
  ;   Class relation predicates
  ;
  (import (scheme base)
          (os meta accessors) )

  (export subclass? nonstrict-subclass?)

  (begin

    (define (subclass? class superclass)
      (if (memq superclass (all-superclasses class)) #t #f) )

    (define (nonstrict-subclass? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
