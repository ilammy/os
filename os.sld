(define-library (os)
  ;
  ;   User layer of OS
  ;
  (import (os accessors)
          (os boot initialized-classes)
          (os callables)
          (os class-of)
          (os generics)
          (os inheritance)
          (os instantiation)
          (os methods) )

  (export <object> <class>
          <slot> <effective-slot>
          <generic> <method>
          <procedure>

          make
          make-class
          make-generic

          add-method!
          add-callable!

          name class-of
          direct-superclasses all-superclasses
          direct-slots all-slots
          init-keyword
          getter setter
          signature
          discriminators
          methods )

)
