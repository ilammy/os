#!r6rs
(library (os protocols inheritance)
  ;
  ;   Inheritance metaobject protocol (class initialization)
  ;
  (export compute-all-superclasses
          compute-all-slots
            compute-effective-slot
          compute-instance-size
          finalize-slot-descriptors!
            compute-direct-slot-accessors )

  (import (os boot meta generics)) )
