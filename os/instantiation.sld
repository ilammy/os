(define-library (os instantiation)
  ;
  ;   Object instantiation
  ;
  (import (scheme base)
          (srfi 2) ; and-let*
          (os accessors)
          (os class-of)
          (os primitives)
          (only (os initargs) get-initarg) )

  (export make)

  (begin

    (define (make class . initargs)
      (let ((object (make-primitive class (instance-size class))))
        (initialize-object! object initargs)
        object ) )

    (define (initialize-object! object initargs)
      (for-each (lambda (slot)
                  (init-slot-with-initargs! slot object initargs) )
        (all-slots (class-of object)) ) )

    (define (init-slot-with-initargs! slot object initargs)
      (and-let* ((keyword (init-keyword slot)))
        (let-values (((keyword-found? value) (get-initarg keyword initargs)))
          (when keyword-found?
            (let ((slot-set! (direct-setter slot)))
              (slot-set! object value) ) ) ) ) )

) )
