(define-library (os boot internal make-method)
  ;
  ;   Defining predefined methods
  ;
  (import (scheme base)
          (only (srfi 1) filter-map)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils initargs) )

  (export make-method)

  (begin

    (define <method>-instance-size 3)

    (define (make-method . initargs)
      (let ((method (make-primitive <method> <method>-instance-size)))
        (initialize-method! method initargs)
        method ) )

    (define (initialize-method! method initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
            ((signature:)   (method-signature-set! method value))
            ((method-body:) (method-body-set!      method value))
            (else (assert #f msg: "Unknown init keyword used for <method>:" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (method-signature-ref method)))
              (not (undefined-slot-value? (method-body-ref method)))
              msg: "Required slots of a <method> are not initialized" )

      (method-discriminators-set! method
        (filter-discriminators (method-signature-ref method)) ) )

    (define (filter-discriminators signature)
      (filter-map
        (lambda (spec) (if (pair? spec) (cadr spec) #f))
        signature ) )
) )
