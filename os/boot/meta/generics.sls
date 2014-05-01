#!r6rs
(library (os boot meta generics)
  ;
  ;   Metaobject definitions
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
              compute-method-function )

  (import (except (rnrs base) assert)
          (os boot macros predefine-generic)
          (os boot meta classes) )

  (begin

    ; protocols/instantiation
    ;
    (predefine-generic make       `((class  ,<class>) . initargs))
    (predefine-generic allocate   `((class  ,<class>)   initargs))
    (predefine-generic initialize `((object ,<object>)  initargs))

    ; protocols/slot-access
    ;
    (predefine-generic slot-ref-in-class    `((class ,<class>) object slot-name))
    (predefine-generic slot-set-in-class!   `((class ,<class>) object slot-name value))
    (predefine-generic slot-bound-in-class? `((class ,<class>) object slot-name))

    ; protocols/inheritance
    ;
    (predefine-generic compute-all-superclasses      `((class ,<class>)))
    (predefine-generic compute-all-slots             `((class ,<class>)))
    (predefine-generic compute-effective-slot        `((class ,<class>) slots))
    (predefine-generic compute-instance-size         `((class ,<class>)))
    (predefine-generic finalize-slot-descriptors!    `((class ,<class>)))
    (predefine-generic compute-direct-slot-accessors `((class ,<class>) (slot ,<effective-slot>)))

    ; protocols/generic-calls
    ;
    (predefine-generic add-method!                `((generic ,<generic>) (method ,<method>)))
    (predefine-generic compute-effective-function `((generic ,<generic>)))
    (predefine-generic find-applicable-methods    `((generic ,<generic>) classes))
    (predefine-generic more-specific-method?      `((method ,<method>) (method ,<method>) classes))
    (predefine-generic compute-effective-method   `((combinator ,<method-combinator>) methods))
    (predefine-generic compute-method-function    `((method ,<method>)))

) )
