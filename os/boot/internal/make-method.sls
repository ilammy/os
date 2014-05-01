#!r6rs
(library (os boot internal make-method)
  ;
  ;   Defining predefined methods
  ;
  (export make-method)

  (import (except (rnrs base) assert)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot internal signature-checks)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

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

      (assert (valid-signature? (method-signature-ref method)))

      (method-discriminators-set! method
        (filter-discriminators (method-signature-ref method)) ) )

    (define (filter-discriminators signature)
      (proper-filter-map
        (lambda (spec) (if (list? spec) (cadr spec) #f))
        signature ) )

) )
