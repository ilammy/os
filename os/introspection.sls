#!r6rs
(library (os introspection)
  ;
  ;   Generic specialization bootstrapping
  ;
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
          abstract?
          init-keyword
          init-value
          init-thunk
          init-required?
          getter
          setter
          signature
          method-class
          method-combinator
          discriminators
          all-superclasses
          all-slots
          methods )

  (import (os boot complete)
          (os meta accessors)
          (os meta classes)
          (os internal class-of) ) )
