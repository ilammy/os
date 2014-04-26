#!r6rs
(library (os macros define-method)
  ;
  ;   Generic maintenance
  ;
  (export define-method)

  (import (rnrs base)
          (os meta classes)
          (os protocols generic-calls)
          (os protocols instantiation) )

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
