(define-library (os protocols inheritance)
  ;
  ;   Inheritance metaobject protocol (class initialization)
  ;
  (import (os boot meta generics))

  (export compute-all-superclasses
          compute-all-slots
            compute-effective-slot
          compute-instance-size
          finalize-slot-descriptors!
            compute-direct-slot-accessors ) )
