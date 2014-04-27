#!r6rs
(library (os boot protocols instantiation)
  ;
  ;   Implementation of protocols/instantiation
  ;
  (export make
            allocate
            initialize )

  (import (except (rnrs base) assert)
          (rnrs control)
          (os meta accessors)
          (os internal class-of)
          (os internal primitives)
          (os internal slot-access)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils initargs) )

  (begin

    (predefine-method (make $ class . initargs) (<class>)
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
      (define (init-with-keyword keyword)
        (let-values (((keyword-found? value) (get-initarg keyword initargs)))
          (if keyword-found?
              (values #t value)
              (init-with-default-value) ) ) )

      (define (init-with-default-value)
        (if (slot-bound? slot 'init-value)
            (values #t (init-value slot))
            (if (slot-bound? slot 'init-thunk)
                (values #t ((init-thunk slot)))
                (values #f #f) ) ) )

      (define (compute-init-value)
        (let ((keyword (init-keyword slot)))
          (if keyword
              (init-with-keyword keyword)
              (init-with-default-value) ) ) )

      (let-values (((can-init? value) (compute-init-value)))
        (if can-init?
            (let ((slot-set! (direct-setter slot)))
              (slot-set! object value) )
            (when (init-required? slot)
              (error #f "no init value provided for a required slot"
                     (name (class-of object)) (name slot) ) ) ) ) )

    'dummy
) )
