(define-library (os boot initialized-classes)

  (import (scheme base)
          (os boot classes definitions)
          (os boot classes initialize)
          (os boot slots layout) )

  (export <object>
          <class>
          <slot>
          <effective-slot>
          <procedure>
          <generic>
          <method> )

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

) )
