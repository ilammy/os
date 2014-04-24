(define-library (os macros define-generic)
  ;
  ;   Generic initialization
  ;
  (import (scheme base)
          (os meta classes)
          (os protocols instantiation)
          (os internal callables)
          (os boot meta accessors) )

  (export define-generic)

  (begin

    (define-syntax define-generic
      (syntax-rules ()
        ((_ name signature)
         (define name
           (let* ((gf-object (make <generic>
                               'name:             'name
                               'signature:        'signature ))
                  (gf-wrapper (lambda args
                                (apply (generic-effective-function-ref gf-object)
                                       args ) )) )
             (add-callable! gf-wrapper gf-object)
             gf-wrapper ) ) ) ) )

) )
