#!r6rs
(library (os boot strap generics)
  ;
  ;   Generic specialization bootstrapping
  ;
  (export make
            allocate
            initialize
              initialize-with-initargs-in-class!

          slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class?

          compute-all-superclasses
          compute-all-slots
            compute-effective-slot
            install-direct-accessors!
          compute-instance-size

          add-method!
            compute-effective-function
              find-applicable-methods
                more-specific-method?
              compute-effective-method
                compute-method-function )

  (import (os boot meta generics)
          (os boot protocols inheritance)
          (os boot protocols instantiation)
          (os boot protocols initialization)
          (os boot protocols generic-calls)
          (os boot protocols slot-access) ) )
