#!r6rs
(library (os boot strap classes)

  (export <object>
          <class>
          <slot>
          <effective-slot>
          <procedure>
          <generic>
          <method>
          <method-combinator>
          <linear-method-combinator>
          <standard-method-combinator> )

  (import (except (rnrs base) assert)
          (os boot meta classes)
          (os boot internal initialize-class)
          (os boot internal slot-layout) )

  (begin

    (define-syntax init-class!
      (syntax-rules (abstract)
        ((_ name (superclasses ...) slots)
         (initialize-class! name
           'name:                'name
           'direct-superclasses: (list superclasses ...)
           'direct-slots:        slots ) )
        ((_ name (superclasses ...) abstract slots)
         (initialize-class! name
           'name:                'name
           'direct-superclasses: (list superclasses ...)
           'direct-slots:        slots
           'abstract:            #t ) ) ) )

    (init-class! <object> ()            abstract direct-<object>-slots)
    (init-class! <class> (<object>)              direct-<class>-slots)
    (init-class! <slot> (<object>)               direct-<slot>-slots)
    (init-class! <effective-slot> (<slot>)       direct-<effective-slot>-slots)
    (init-class! <procedure> (<object>) abstract direct-<procedure>-slots)
    (init-class! <generic> (<procedure>)         direct-<generic>-slots)
    (init-class! <method> (<object>)             direct-<method>-slots)

    (init-class! <method-combinator> (<object>) abstract
      direct-<method-combinator>-slots )
    (init-class! <linear-method-combinator> (<method-combinator>)
      direct-<linear-method-combinator>-slots )
    (init-class! <standard-method-combinator> (<method-combinator>)
      direct-<standard-method-combinator>-slots )

) )
