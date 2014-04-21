(define-library (os boot macros predefine-generic)

  (import (scheme base)
          (os internal callables)
          (os boot internal make-generic)
          (os boot meta accessors) )

  (export predefine-generic)

  (begin

    (define-syntax predefine-generic
      (syntax-rules ()
        ((_ name signature)
         (define name
           (let* ((gf-object (make-generic
                               'name:             'name
                               'signature:        'signature ))
                  (gf-wrapper (lambda args
                                (apply (generic-effective-function-ref gf-object)
                                       args ) )) )
             (add-callable! gf-wrapper gf-object)
             gf-wrapper ) ) ) ) )

) )
