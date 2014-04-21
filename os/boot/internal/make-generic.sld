(define-library (os boot internal make-generic)
  ;
  ;   Defining predefined generic functions
  ;
  (import (scheme base)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils initargs) )

  (export make-generic)

  (begin

    (define <generic>-instance-size                  5)
    (define <linear-method-combinator>-instance-size 0)

    (define (make-default-method-combinator)
      (make-primitive <linear-method-combinator>
                      <linear-method-combinator>-instance-size ) )

    (define (make-generic . initargs)
      (let ((generic (make-primitive <generic> <generic>-instance-size)))
        (initialize-generic! generic initargs)
        generic ) )

    (define (initialize-generic! generic initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
            ((name:)              (generic-name-set!      generic value))
            ((signature:)         (generic-signature-set! generic value))
            (else (error "unknown init keyword" "<generic>" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (generic-name-ref      generic)))
              (not (undefined-slot-value? (generic-signature-ref generic))) )

      (generic-method-combinator-set! generic (make-default-method-combinator))
      (generic-methods-set! generic '())
      (generic-effective-function-set! generic
        (lambda args
          (error "no applicable method" (generic-name-ref generic) args) ) ) )

) )
