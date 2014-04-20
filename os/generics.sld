(define-library (os generics)
  ;
  ;   Generic initialization
  ;
  (import (scheme base)
          (os callables)
          (os instantiation)
          (os boot accessors)
          (os boot initialized-classes) )

  (export make-generic define-generic)

  (begin

    (define (make-generic . initargs)
      (let ((generic (apply make <generic> initargs)))
        (generic-methods-set! generic '())
        (generic-effective-function-set! generic
          (lambda args
            (error "no applicable method" (generic-name-ref generic) args) ) )
        generic ) )

    (define-syntax define-generic
      (syntax-rules ()
        ((_ name signature)
         (define name
           (let* ((gf-object (make-generic
                               'name:             'name
                               'signature:        'signature
                               'method-combinator: (make <linear-method-combinator>) ))
                  (gf-wrapper (lambda args
                                (apply (generic-effective-function-ref gf-object)
                                       args ) )) )
             (add-callable! gf-wrapper gf-object)
             gf-wrapper ) ) ) ) )

) )
