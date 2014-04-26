#!r6rs
(library (os internal class-of)
  ;
  ;   Classifier
  ;
  (export class-of)

  (import (except (rnrs base) assert)
          (os meta classes)
          (os internal callables)
          (os internal primitives)
          (os boot meta accessors) )

  (begin

    (define (class-of object)
      (cond ((primitive? object) (object-class-ref object))
            ((callable?  object) (object-class-ref (object-of object)))
            ((procedure? object) <procedure>)
            (else (error #f "unsupported object type" object)) ) )

) )
