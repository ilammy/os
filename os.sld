(define-library (os)
  ;
  ;   User layer of OS
  ;
  (import (os accessors)
          (os boot initialized-classes)
          (os boot specialized-generics)
          (os class-of)
          (os generics)
          (os methods)
          (os slot-access)
          (os protocols instantiation) )

  (export <object> <class>
          <slot> <effective-slot>
          <generic> <method>
          <procedure>

          make

          define-generic
          define-method

          name class-of
          direct-superclasses all-superclasses
          direct-slots all-slots
          init-keyword
          getter setter
          signature
          discriminators
          methods

          slot-ref
          slot-set! )

)
