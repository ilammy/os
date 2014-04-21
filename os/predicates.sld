(define-library (os predicates)
  ;
  ;   Class relation predicates
  ;
  (import (scheme base)
          (os accessors) )

  (export subclass? instance-of?)

  (begin

    (define (subclass? class superclass)
      (if (memq superclass (all-superclasses class)) #t #f) )

    (define (instance-of? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
