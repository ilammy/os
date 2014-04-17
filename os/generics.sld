(define-library (os generics)
  ;
  ;   Generic initialization
  ;
  (import (scheme base)
          (os accessors)
          (os instantiation)
          (os boot initialized-classes) )

  (export make-generic)

  (begin

    (define (make-generic . initargs)
      (let ((generic (apply make <generic> initargs)))
        (set-methods! generic '())
        (set-effective-function! generic
          (lambda args (error "no applicable method" (name generic) args)) )

        generic ) )

) )
