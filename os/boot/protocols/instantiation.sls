#!r6rs
(library (os boot protocols instantiation)
  ;
  ;   Implementation of protocols/instantiation
  ;
  (export make
            allocate
            initialize
              initialize-with-initargs-in-class! )

  (import (except (rnrs base) assert)
          (rnrs control)
          (os meta accessors)
          (os internal class-of)
          (os internal primitives)
          (os internal slot-access)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils assert)
          (os utils initargs) )

  (begin

    (predefine-method (make $ class . initargs) `((class ,<class>) . initargs)
      (initialize (allocate class initargs) initargs) )

    (predefine-method (allocate $ class initargs) `((class ,<class>) initargs)
      (make-primitive class (instance-size class)) )

    (predefine-method (initialize $ object initargs) `((object ,<object>) initargs)
      (let ((class (class-of object)))
        (for-each
          (lambda (slot)
            (initialize-with-initargs-in-class! class slot object initargs) )
          (all-slots class) ) )
      object )

    (predefine-method (initialize-with-initargs-in-class! $ class slot object initargs)
                      `((class ,<class>) slot object initargs)
      (define (init-with-keyword keyword)
        (let-values (((keyword-found? value) (get-initarg keyword initargs)))
          (if keyword-found?
              (values 'with-keyword value)
              (init-with-default-value) ) ) )

      (define (init-with-default-value)
        (if (slot-bound? slot 'init-value)
            (values 'default (init-value slot))
            (if (slot-bound? slot 'init-thunk)
                (values 'default ((init-thunk slot)))
                (values #f #f) ) ) )

      (define (compute-init-value)
        (let ((keyword (init-keyword slot)))
          (if keyword
              (init-with-keyword keyword)
              (init-with-default-value) ) ) )

      (define (init-slot-with! value)
        (let ((slot-set! (direct-setter slot)))
          (slot-set! object value) ) )

      (define (slot-is-not-initialized?)
        (let ((slot-get (direct-getter slot)))
          (undefined-slot-value? (slot-get object)) ) )

      (let-values (((can-init value) (compute-init-value)))
        (if can-init
            (case (allocation slot)
              ((instance) (init-slot-with! value))
              ((class each-subclass)
               (when (or (slot-is-not-initialized?)
                         (eq? can-init 'with-keyword) )
                 (init-slot-with! value) ) )
              (else (assert #f msg: "unexpected invalid slot allocation"
                                    (name class) (name slot) (allocation slot) )) )
            (when (init-required? slot)
              (error #f "no init value provided for a required slot"
                     (name (class-of object)) (name slot) ) ) ) ) )

    'dummy
) )
