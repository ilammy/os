(define-library (os generics)
  ;
  ;   Generic initialization
  ;
  (import (scheme base)
          (os callables)
          (os boot accessors)
          (os boot initialized-classes)
          (os protocols instantiation) )

  (export define-generic)

  (begin

    (define-syntax define-generic
      (syntax-rules ()
        ((_ name signature)
         (define name
           (let* ((gf-object (make <generic>
                               'name:             'name
                               'signature:        'signature
                               'method-combinator: (make <linear-method-combinator>) ))
                  (gf-wrapper (lambda args
                                (apply (generic-effective-function-ref gf-object)
                                       args ) )) )
             (add-callable! gf-wrapper gf-object)
             gf-wrapper ) ) ) ) )

) )
