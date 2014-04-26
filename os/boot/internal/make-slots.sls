#!r6rs
(library (os boot internal make-slots)
  ;
  ;   Bootstrapping constructors for slots
  ;
  (export make-slot make-effective-slot)

  (import (rnrs base)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

  (begin

    (define <slot>-instance-size           7)
    (define <effective-slot>-instance-size 9)

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
            ((name:)          (slot-name-set!          slot value))
            ((init-keyword:)  (slot-init-keyword-set!  slot value))
            ((init-required:) (slot-init-required-set! slot value))
            ((init-value:)    (slot-init-value-set!    slot value))
            ((init-thunk:)    (slot-init-thunk-set!    slot value))
            ((getter:)        (slot-getter-set!        slot value))
            ((setter:)        (slot-setter-set!        slot value))
            (else (assert #f msg: "Unknown init keyword used for <slot>:" key)) ) )
        initargs )

      (when (undefined-slot-value? (slot-init-keyword-ref slot))
        (slot-init-keyword-set! slot #f) )

      (when (undefined-slot-value? (slot-init-required-ref slot))
        (slot-init-required-set! slot #f) )

      (when (undefined-slot-value? (slot-getter-ref slot))
        (slot-getter-set! slot #f) )

      (when (undefined-slot-value? (slot-setter-ref slot))
        (slot-setter-set! slot #f) )

      (assert (not (undefined-slot-value? (slot-name-ref slot)))
              msg: "Required slots of a <slot> are not initialized" )

      (let ((has-init-value (not (undefined-slot-value? (slot-init-value-ref slot))))
            (has-init-thunk (not (undefined-slot-value? (slot-init-thunk-ref slot))))
            (init-required  (slot-init-required-ref slot))
            (init-keyword   (slot-init-keyword-ref slot)) )

        (assert (not (and has-init-value has-init-thunk))
                msg: "A slot cannot have both init-value and init-thunk defined" )

        (assert (implies has-init-thunk (procedure? (slot-init-thunk-ref slot)))
                msg: "Init thunk must be a procedure" )

        (assert (implies init-required init-keyword)
                msg: "A slot cannot be required without having a keyword" )

        (assert (implies init-required (not (or has-init-value has-init-thunk)))
                msg: "A slot cannot be required while having a default value" ) ) )

) )
