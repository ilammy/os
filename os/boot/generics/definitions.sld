(define-library (os boot generics definitions)
  ;
  ;   Metaobject definitions
  ;
  (import (scheme base)
          (os boot generics predefine) )

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

  (begin

    ; protocols/instantiation
    ;
    (predefine-generic make       ((class) initargs))
    (predefine-generic allocate   ((class) initargs))
    (predefine-generic initialize ((object) initargs))

    ; protocols/slot-access
    ;
    (predefine-generic slot-ref-in-class    ((object) (class) slot-name))
    (predefine-generic slot-set-in-class!   ((object) (class) slot-name value))
    (predefine-generic slot-bound-in-class? ((object) (class) slot-name))

    ; protocols/inheritance
    ;
    (predefine-generic compute-all-superclasses      ((class)))
    (predefine-generic compute-all-slots             ((class)))
    (predefine-generic compute-effective-slot        ((class) slots))
    (predefine-generic compute-instance-size         ((class)))
    (predefine-generic finalize-slot-descriptors!    ((class)))
    (predefine-generic compute-direct-slot-accessors ((class) (slot)))

    ; protocols/generic-calls
    ;
    (predefine-generic add-method!                ((generic) (method)))
    (predefine-generic compute-effective-function ((generic)))
    (predefine-generic find-applicable-methods    ((generic) classes))
    (predefine-generic more-specific-method?      ((method) (method) classes))
    (predefine-generic compute-effective-method   ((combinator) methods))
    (predefine-generic compute-method-function    ((method)))

) )
