#!r6rs
(library (os boot meta classes)
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
          <standard-method-combinator> )

  (import (except (rnrs base) assert)
          (os internal primitives) )

  (begin

    (define <class>-instance-size 7)

    ;; Metaobject classes
    (define <class>          (make-primitive-<class> <class>-instance-size))
    (define <object>         (make-primitive <class> <class>-instance-size))
    (define <slot>           (make-primitive <class> <class>-instance-size))
    (define <effective-slot> (make-primitive <class> <class>-instance-size))
    (define <generic>        (make-primitive <class> <class>-instance-size))
    (define <method>         (make-primitive <class> <class>-instance-size))

    ;; Built-in classes
    (define <procedure>      (make-primitive <class> <class>-instance-size))

    ;; Method combinators
    (define <method-combinator>          (make-primitive <class> <class>-instance-size))
    (define <standard-method-combinator> (make-primitive <class> <class>-instance-size))

) )
