#!r6rs
(library (os full-mop)
  ;
  ;   Generic specialization bootstrapping
  ;
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
                compute-method-function

          to-write
          to-display )

  (import (os boot complete)
          (os protocols generic-calls)
          (os protocols inheritance)
          (os protocols instantiation)
          (os protocols slot-access)
          (os protocols pretty-printing) ) )
