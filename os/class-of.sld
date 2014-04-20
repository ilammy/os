(define-library (os class-of)
  ;
  ;   Classifier
  ;
  (import (scheme base)
          (os primitives)
          (os callables)
          (os boot accessors)
          (os boot classes definitions) )

  (export class-of)

  (begin

    (define (class-of object)
      (cond ((primitive? object) (object-class-ref object))
            ((callable?  object) (object-class-ref (object-of object)))
            ((procedure? object) <procedure>)
            (else (error "unsupported object type" object)) ) )

) )
