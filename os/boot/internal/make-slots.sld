(define-library (os boot internal make-slots)
  ;
  ;   Bootstrapping constructors for slots
  ;
  (import (scheme base)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils initargs) )

  (export make-slot make-effective-slot)

  (begin

    (define <slot>-instance-size           6)
    (define <effective-slot>-instance-size 8)

    (define (make-slot . initargs)
      (let ((slot (make-primitive <slot> <slot>-instance-size)))
        (initialize-slot! slot initargs)
        slot ) )

    (define (make-effective-slot . initargs)
      (let ((effective-slot (make-primitive <effective-slot>
                                            <effective-slot>-instance-size )))
        (initialize-slot! effective-slot initargs)
        effective-slot ) )

    (define (initialize-slot! slot initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
            ((name:)         (slot-name-set!         slot value))
            ((init-keyword:) (slot-init-keyword-set! slot value))
            ((init-value:)   (slot-init-value-set!   slot value))
            ((init-thunk:)   (slot-init-thunk-set!   slot value))
            ((getter:)       (slot-getter-set!       slot value))
            ((setter:)       (slot-setter-set!       slot value))
            (else (error "unknown init keyword" "<slot>" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (slot-name-ref slot))))

      (when (undefined-slot-value? (slot-init-keyword-ref slot))
        (slot-init-keyword-set! slot #f) )

      (when (undefined-slot-value? (slot-getter-ref slot))
        (slot-getter-set! slot #f) )

      (when (undefined-slot-value? (slot-setter-ref slot))
        (slot-setter-set! slot #f) ) )

) )
