#!r6rs
(library (os boot macros predefine-method)

  (export predefine-method)

  (import (except (rnrs base) assert)
          (os boot internal generic-calls)
          (os boot internal make-method) )

  (begin

    (define-syntax predefine-method
      (syntax-rules ()
        ((_ (generic call-next-method . args) (specializers ...) body1 body2 ...)
         (generic-add-method! generic
           (make-method
             'discriminators: (list specializers ...)
             'method-body:
               (lambda (call-next-method . args)
                 body1 body2 ... ) ) ) ) ) )

) )
