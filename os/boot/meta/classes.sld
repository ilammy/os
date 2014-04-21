(define-library (os boot meta classes)
  ;
  ;   Metaobject definitions
  ;
  (import (scheme base)
          (os internal primitives) )

  (export <class>
          <object>
          <slot>
          <effective-slot>
          <generic>
          <method>
          <procedure>
          <method-combinator>
          <linear-method-combinator> )

  (begin

    ;; Metaobject classes
    (define <class>          (make-primitive-<class> 6))
    (define <object>         (make-primitive <class> 6))
    (define <slot>           (make-primitive <class> 6))
    (define <effective-slot> (make-primitive <class> 6))
    (define <generic>        (make-primitive <class> 6))
    (define <method>         (make-primitive <class> 6))

    ;; Built-in classes
    (define <procedure>      (make-primitive <class> 6))

    ;; Method combinators
    (define <method-combinator>        (make-primitive <class> 6))
    (define <linear-method-combinator> (make-primitive <class> 6))

) )
