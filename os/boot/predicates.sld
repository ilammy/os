(define-library (os boot predicates)
  ;
  ;   Class relation predicates
  ;
  (import (scheme base)
          (os assert)
          (os class-of)
          (os boot accessors)
          (os boot classes definitions) )

  (export subclass? instance-of?)

  (begin

    (define (subclass? class superclass)
      (assert (eq? <class> (class-of class))
              (eq? <class> (class-of superclass)) )
      (if (memq superclass (class-all-superclasses-ref class)) #t #f) )

    (define (instance-of? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
