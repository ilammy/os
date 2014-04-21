(define-library (os meta classes)
  ;
  ;   Metaobject definitions
  ;
  (import (os boot meta classes))

  (export <class>
          <object>
          <slot>
          <effective-slot>
          <generic>
          <method>
          <procedure>
          <method-combinator>
          <linear-method-combinator> ) )
