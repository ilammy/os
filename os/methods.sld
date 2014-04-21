(define-library (os methods)
  ;
  ;   Generic maintenance
  ;
  (import (scheme base)
          (os boot classes definitions)
          (os protocols generic-calls)
          (os protocols instantiation) )

  (export add-method! define-method)

  (begin

    (define-syntax define-method
      (syntax-rules ()
        ((_ (generic call-next-method args ...) (specializers ...) body1 body2 ...)
         (add-method! generic
           (make <method>
             (list 'discriminators: (list specializers ...)
                   'method-body:
                     (lambda (call-next-method args ...)
                       body1 body2 ... ) ) ) ) ) ) )

) )
