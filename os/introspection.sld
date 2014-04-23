(define-library (os introspection)
  ;
  ;   Generic specialization bootstrapping
  ;
  (import (os boot complete)
          (os meta accessors)
          (os meta classes)
          (os internal class-of) )

  (export class-of

          <class>
          <object>
          <slot>
          <effective-slot>
          <generic>
          <method>
          <procedure>
          <method-combinator>
          <linear-method-combinator>

          name
          direct-superclasses
          direct-slots
          init-keyword
          init-value
          init-thunk
          init-required?
          getter
          setter
          signature
          method-combinator
          discriminators
          all-superclasses
          all-slots
          methods ) )
