#!r6rs
(library (os meta classes)
  ;
  ;   Metaobject definitions
  ;
  (export <class>
          <object>
          <slot>
          <effective-slot>
          <generic>
          <method>
          <procedure>
          <method-combinator>
          <linear-method-combinator>
          <standard-method-combinator> )

  (import (os boot meta classes)) )
