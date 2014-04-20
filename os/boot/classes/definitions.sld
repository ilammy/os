(define-library (os boot classes definitions)
  ;
  ;   Metaobject definitions
  ;
  (import (scheme base)
          (os primitives) )

  (export <class>
          <object>
          <slot>
          <effective-slot>
          <generic>
          <method>
          <procedure> )

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

) )
