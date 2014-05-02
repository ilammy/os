#!r6rs
(library (os macros define-method)
  ;
  ;   Generic maintenance
  ;
  (export define-method)

  (import (except (rnrs base) assert)
          (os meta accessors)
          (os meta classes)
          (os protocols generic-calls)
          (os protocols instantiation) )

  (begin

    (define-syntax define-method
      (syntax-rules ()
        ((_ (generic call-next-method . args) signature body1 body2 ...)
         (add-method! generic
           (make (method-class generic)
             'signature: signature
             'method-body:
               (lambda (call-next-method . args)
                 body1 body2 ... ) ) ) ) ) )

) )
