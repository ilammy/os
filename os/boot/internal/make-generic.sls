#!r6rs
(library (os boot internal make-generic)
  ;
  ;   Defining predefined generic functions
  ;
  (export make-generic)

  (import (except (rnrs base) assert)
          (rnrs control)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot internal signature-checks)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

  (begin

    (define <generic>-instance-size                  7)
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

      (assert (valid-signature? (generic-signature-ref generic)))

      (when (undefined-slot-value? (generic-name-ref generic))
        (generic-name-set! generic (string->symbol "#<anonymous>")) )

      (generic-methods-set!           generic '())
      (generic-method-class-set!      generic <method>)
      (generic-method-combinator-set! generic (make-default-method-combinator))

      (generic-effective-function-set! generic
        (lambda args
          (error #f "no applicable method" (generic-name-ref generic) args) ) )

      (generic-significant-positions-set! generic
        (calculate-significant-positions (generic-signature-ref generic)) ) )

    (define (calculate-significant-positions signature)
      (indices-of list? signature ) )

) )
