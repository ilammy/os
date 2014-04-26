#!r6rs
(library (os boot internal make-method)
  ;
  ;   Defining predefined methods
  ;
  (export make-method)

  (import (rnrs base)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils initargs) )

  (begin

    (define <method>-instance-size 2)

    (define (make-method . initargs)
      (let ((method (make-primitive <method> <method>-instance-size)))
        (initialize-method! method initargs)
        method ) )

    (define (initialize-method! method initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
            ((discriminators:) (method-discriminators-set! method value))
            ((method-body:)    (method-body-set!           method value))
            (else (assert #f msg: "Unknown init keyword used for <method>:" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (method-discriminators-ref method)))
              (not (undefined-slot-value? (method-body-ref method)))
              msg: "Required slots of a <method> are not initialized" ) )

) )
