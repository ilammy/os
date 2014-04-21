(define-library (os macros define-method)
  ;
  ;   Generic maintenance
  ;
  (import (scheme base)
          (os meta classes)
          (os protocols generic-calls)
          (os protocols instantiation) )

  (export define-method)

  (begin

    (define-syntax define-method
      (syntax-rules ()
        ((_ (generic call-next-method . args) (specializers ...) body1 body2 ...)
         (add-method! generic
           (make <method>
             'discriminators: (list specializers ...)
             'method-body:
               (lambda (call-next-method . args)
                 body1 body2 ... ) ) ) ) ) )

) )
