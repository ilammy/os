(define-library (os boot specialized-generics)
  ;
  ;   Generic specialization bootstrapping
  ;
  (import (scheme base)
          (os boot generics definitions)
          (os boot methods inheritance)
          (os boot methods instantiation)
          (os boot methods initialization)
          (os boot methods generic-calls)
          (os boot methods slot-access) )

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
                compute-method-function )

)
