(define-library (os boot slot-layout)
  ;
  ;   Slot layout in predefined metaobjects
  ;
  (import (scheme base)
          (os accessors)
          (os boot slot-makers)
          (os boot class-definitions) )

  (export all-slots-of
          direct-<object>-slots
          direct-<class>-slots
          direct-<slot>-slots
          direct-<effective-slot>-slots
          direct-<procedure>-slots
          direct-<generic>-slots
          direct-<method>-slots )

  (begin

    (define direct-<object>-slots '())

    (define direct-<class>-slots
      (list
        (make-slot 'name:         'name
                   'init-keyword: 'name:
                   'getter:        name )

        (make-slot 'name:         'direct-superclasses
                   'init-keyword: 'direct-superclasses:
                   'getter:        direct-superclasses )

        (make-slot 'name:         'direct-slots
                   'init-keyword: 'direct-slots:
                   'getter:        direct-slots )

        (make-slot 'name:         'all-superclasses
                   'getter:        all-superclasses )

        (make-slot 'name:         'all-slots
                   'getter:        all-slots )

        (make-slot 'name:         'instance-size) ) )

    (define direct-<slot>-slots
      (list
        (make-slot 'name:         'name
                   'init-keyword: 'name:
                   'getter:        name )

        (make-slot 'name:         'init-keyword
                   'init-keyword: 'init-keyword:
                   'getter:        init-keyword )

        (make-slot 'name:         'getter
                   'init-keyword: 'getter:
                   'getter:        getter )

        (make-slot 'name:         'setter
                   'init-keyword: 'setter:
                   'getter:        setter ) ) )

    (define direct-<effective-slot>-slots
      (list
        (make-slot 'name: 'direct-getter)
        (make-slot 'name: 'direct-setter) ) )

    (define direct-<generic>-slots
      (list
        (make-slot 'name:         'name
                   'init-keyword: 'name:
                   'getter:        name )

        (make-slot 'name:         'signature
                   'init-keyword: 'signature:
                   'getter:        signature )

        (make-slot 'name:         'methods
                   'getter:        methods )

        (make-slot 'name:         'effective-function) ) )

    (define direct-<method>-slots
      (list
        (make-slot 'name:         'discriminators
                   'init-keyword: 'discriminators:
                   'getter:        discriminators )

        (make-slot 'name:         'method-body
                   'init-keyword: 'method-body: ) ) )

    (define direct-<procedure>-slots '())

    (define all-<object>-slots         direct-<object>-slots)
    (define all-<class>-slots          (append all-<object>-slots direct-<class>-slots))
    (define all-<slot>-slots           (append all-<object>-slots direct-<slot>-slots))
    (define all-<effective-slot>-slots (append all-<slot>-slots   direct-<effective-slot>-slots))
    (define all-<procedure>-slots      (append all-<object>-slots direct-<procedure>-slots))
    (define all-<generic>-slots        (append all-<object>-slots direct-<generic>-slots))
    (define all-<method>-slots         (append all-<object>-slots direct-<method>-slots))

    (define (all-slots-of class)
      (cond ((eq? class <object>)         all-<object>-slots)
            ((eq? class <class>)          all-<class>-slots)
            ((eq? class <slot>)           all-<slot>-slots)
            ((eq? class <effective-slot>) all-<effective-slot>-slots)
            ((eq? class <procedure>)      all-<procedure>-slots)
            ((eq? class <generic>)        all-<generic>-slots)
            ((eq? class <method>)         all-<method>-slots) ) )

) )
