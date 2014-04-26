#!r6rs
(library (os internal primitives)
  ;
  ;   Primitive objects
  ;
  (export make-primitive
          make-primitive-<class>
          primitive?
          primitive-class
          primitive-ref
          primitive-set!
          undefined-slot-value? )

  (import (rnrs base))

  (begin
 
    (define-record-type undefined-value
      (make-undefined-value)
      undefined-slot-value? )

    (define *undefined-slot-value* (make-undefined-value))

    (define-record-type primitive-object
      (make-primitive-object class slots)
      primitive?
      (class primitive-class set-primitive-class!)
      (slots primitive-slots) )

    (define (make-primitive class slot-count)
      (make-primitive-object class
       (make-vector slot-count *undefined-slot-value*) ) )

    (define (make-primitive-<class> slot-count)
      (let ((primitive (make-primitive #f slot-count)))
        (set-primitive-class! primitive primitive)
        primitive ) )

    (define (primitive-ref primitive nth)
      (vector-ref (primitive-slots primitive) nth) )

    (define (primitive-set! primitive nth value)
      (vector-set! (primitive-slots primitive) nth value) )

) )
