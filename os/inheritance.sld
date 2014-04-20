(define-library (os inheritance)
  ;
  ;   Class inheritance
  ;
  (import (scheme base)
          (os accessors)
          (os instantiation)
          (os boot classes definitions)
          (os protocols inheritance) )

  (export make-class)

  (begin

    (define (make-class . initargs)
      (let ((class (apply make <class> initargs)))
        (set-all-superclasses! class (compute-all-superclasses class))
        (set-all-slots!        class (compute-all-slots class))
        (set-instance-size!    class (compute-instance-size class))
        (finalize-slot-descriptors! class)
        class ) )

) )
