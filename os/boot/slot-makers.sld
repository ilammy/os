(define-library (os boot slot-makers)
  ;
  ;   Bootstrapping constructors for slots
  ;
  (import (scheme base)
          (os primitives)
          (os boot accessors)
          (os boot class-definitions)
          (only (os initargs) for-each-initarg) )

  (export make-slot make-effective-slot)

  (begin

    (define <slot>-instance-size           4)
    (define <effective-slot>-instance-size 6)

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
            ((getter:)       (slot-getter-set!       slot value))
            ((setter:)       (slot-setter-set!       slot value))
            (else (error "unknown init keyword" "<slot>" key)) ) )
        initargs ) )

) )
