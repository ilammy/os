#!r6rs
(library (os boot predicates)
  ;
  ;   Class relation predicates
  ;
  (export subclass? nonstrict-subclass?)

  (import (rnrs base)
          (os internal class-of)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert) )

  (begin

    (define (subclass? class superclass)
      (assert (eq? <class> (class-of class))
              (eq? <class> (class-of superclass)) )
      (if (memq superclass (class-all-superclasses-ref class)) #t #f) )

    (define (nonstrict-subclass? class superclass)
      (or (eq? class superclass)
          (subclass? class superclass) ) )

) )
