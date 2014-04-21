(define-library (os boot methods instantiation)
  ;
  ;   Implementation of protocols/instantiation
  ;
  (import (scheme base)
          (srfi 2) ; and-let*
          (os accessors)
          (os class-of)
          (os initargs)
          (os primitives)
          (os boot classes definitions)
          (os boot generics definitions)
          (os boot methods predefine) )

  (export make allocate initialize)

  (begin

    (predefine-method (make $ class initargs) (<class>)
      (initialize (allocate class initargs) initargs) )

    (predefine-method (allocate $ class initargs) (<class>)
      (make-primitive class (instance-size class)) )

    (predefine-method (initialize $ object initargs) (<object>)
      (for-each
        (lambda (slot)
          (init-slot-with-initargs! slot object initargs) )
        (all-slots (class-of object)) )
      object )

    (define (init-slot-with-initargs! slot object initargs)
      (and-let* ((keyword (init-keyword slot)))
        (let-values (((keyword-found? value) (get-initarg keyword initargs)))
          (when keyword-found?
            (let ((slot-set! (direct-setter slot)))
              (slot-set! object value) ) ) ) ) )

) )
