(define-library (os generics)
  ;
  ;   Generic initialization
  ;
  (import (scheme base)
          (os accessors)
          (os callables)
          (os instantiation)
          (os boot initialized-classes) )

  (export make-generic define-generic)

  (begin

    (define (make-generic . initargs)
      (let ((generic (apply make <generic> initargs)))
        (set-methods! generic '())
        (set-effective-function! generic
          (lambda args (error "no applicable method" (name generic) args)) )
        generic ) )

    (define-syntax define-generic
      (syntax-rules ()
        ((_ name signature)
         (define name
           (let* ((gf-object (make-generic
                               'name:      'name
                               'signature: 'signature ))
                  (gf-wrapper (lambda args
                                (apply (effective-function gf-object) args) )) )
             (add-callable! gf-wrapper gf-object)
             gf-wrapper ) ) ) ) )

) )
