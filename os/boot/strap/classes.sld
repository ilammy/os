(define-library (os boot strap classes)

  (import (scheme base)
          (os boot meta classes)
          (os boot internal initialize-class)
          (os boot internal slot-layout) )

  (export <object>
          <class>
          <slot>
          <effective-slot>
          <procedure>
          <generic>
          <method>
          <method-combinator>
          <linear-method-combinator> )

  (begin

    (define-syntax init-class!
      (syntax-rules ()
        ((_ name (superclasses ...) slots)
         (initialize-class! name
           'name:                'name
           'direct-superclasses: (list superclasses ...)
           'direct-slots:        slots ) ) ) )

    (init-class! <object> ()               direct-<object>-slots)
    (init-class! <class> (<object>)        direct-<class>-slots)
    (init-class! <slot> (<object>)         direct-<slot>-slots)
    (init-class! <effective-slot> (<slot>) direct-<effective-slot>-slots)
    (init-class! <procedure> (<object>)    direct-<procedure>-slots)
    (init-class! <generic> (<procedure>)   direct-<generic>-slots)
    (init-class! <method> (<object>)       direct-<method>-slots)

    (init-class! <method-combinator> (<object>)
      direct-<method-combinator>-slots )
    (init-class! <linear-method-combinator> (<method-combinator>)
      direct-<linear-method-combinator>-slots )

) )
