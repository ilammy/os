(define-library (os boot internal make-generic)
  ;
  ;   Defining predefined generic functions
  ;
  (import (scheme base)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

  (export make-generic)

  (begin

    (define <generic>-instance-size                  6)
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
            (else (assert #f msg: "Unknown init keyword used for <generic>:" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (generic-signature-ref generic)))
              msg: "Required slots of a <generic> are not initialized" )

      (when (undefined-slot-value? (generic-name-ref generic))
        (generic-name-set! generic '|#<anonymous>|) )

      (when (undefined-slot-value? (generic-method-combinator-ref generic))
        (generic-method-combinator-set! generic (make-default-method-combinator)) )

      (generic-methods-set! generic '())

      (generic-effective-function-set! generic
        (lambda args
          (error "no applicable method" (generic-name-ref generic) args) ) )

      (generic-significant-positions-set! generic
        (calculate-significant-positions (generic-signature-ref generic)) ) )

    (define (calculate-significant-positions signature)
      (indices-of pair? signature ) )

) )
