#!r6rs
(library (os protocols inheritance)
  ;
  ;   Inheritance metaobject protocol (class initialization)
  ;
  (export compute-all-superclasses
          compute-all-slots
            compute-effective-slot
            install-direct-accessors!
          compute-instance-size )

  (import (os boot meta generics)) )
