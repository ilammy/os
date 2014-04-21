(define-library (os boot generics predefine)
  ;
  ;   Defining predefined generic functions
  ;
  (import (scheme base)
          (os callables)
          (os initargs)
          (os primitives)
          (os boot accessors)
          (os boot classes definitions) )

  (export make-generic predefine-generic)

  (begin

    (define <generic>-instance-size                  5)
    (define <linear-method-combinator>-instance-size 0)

    (define (make-generic . initargs)
      (let ((generic (make-primitive <generic> <generic>-instance-size)))
        (initialize-generic! generic initargs)
        generic ) )

    (define (make-default-method-combinator)
      (make-primitive <linear-method-combinator> <linear-method-combinator>-instance-size) )

    (define-syntax predefine-generic
      (syntax-rules ()
        ((_ name signature)
         (define name
           (let* ((gf-object (make-generic
                               'name:             'name
                               'signature:        'signature
                               'method-combinator: (make-default-method-combinator) ))
                  (gf-wrapper (lambda args
                                (apply (generic-effective-function-ref gf-object)
                                       args ) )) )
             (add-callable! gf-wrapper gf-object)
             gf-wrapper ) ) ) ) )

    (define (initialize-generic! generic initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
            ((name:)              (generic-name-set!              generic value))
            ((signature:)         (generic-signature-set!         generic value))
            ((method-combinator:) (generic-method-combinator-set! generic value))
            (else (error "unknown init keyword" "<generic>" key)) ) )
        initargs )

      (generic-methods-set! generic '())
      (generic-effective-function-set! generic
        (lambda args
          (error "no applicable method" (generic-name-ref generic) args) ) ) )

) )
