(define-library (os protocols inheritance)
  ;
  ;   Inheritance metaobject protocol (class initialization)
  ;
  (import (os boot generics definitions))

  (export compute-all-superclasses
          compute-all-slots
            compute-effective-slot
          compute-instance-size
          finalize-slot-descriptors!
            compute-direct-slot-accessors ) )
