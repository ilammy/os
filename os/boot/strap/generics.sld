(define-library (os boot strap generics)
  ;
  ;   Generic specialization bootstrapping
  ;
  (import (scheme base)
          (os boot meta generics)
          (os boot protocols inheritance)
          (os boot protocols instantiation)
          (os boot protocols initialization)
          (os boot protocols generic-calls)
          (os boot protocols slot-access) )

  (export make
            allocate
            initialize

          slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class?

          compute-all-superclasses
          compute-all-slots
            compute-effective-slot
          compute-instance-size
          finalize-slot-descriptors!
            compute-direct-slot-accessors

          add-method!
            compute-effective-function
              find-applicable-methods
                more-specific-method?
              compute-effective-method
                compute-method-function ) )
