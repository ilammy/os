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

      (define (try-keyword-init-value slot keyword initargs)
        (let-values (((keyword-found? value) (get-initarg keyword initargs)))
          (if keyword-found?
              (values 'with-keyword value)
              (default-init-value slot) ) ) )

      (define (default-init-value slot)
        (cond ((slot-bound? slot 'init-value) (values 'default  (init-value slot)))
              ((slot-bound? slot 'init-thunk) (values 'default ((init-thunk slot))))
              (else (values #f #f)) ) )

      (define (compute-init-value slot initargs)
        (let ((keyword (init-keyword slot)))
          (if keyword
              (try-keyword-init-value slot keyword initargs)
              (default-init-value slot) ) ) )

      (define (set-slot-value! slot object value)
        (let ((slot-set! (direct-setter slot)))
          (slot-set! object value) ) )

      (define (uninitialized? slot object)
        (let ((slot-get (direct-getter slot)))
          (undefined-slot-value? (slot-get object)) ) )

      (define (init-slot! slot object can-init value)
        (case (allocation slot)
          ((instance) (set-slot-value! slot object value))
          ((each-subclass class-lineage)
           (when (or (uninitialized? slot object)
                     (eq? can-init 'with-keyword) )
             (set-slot-value! slot object value) ) )
          (else (assert #f msg: "unexpected invalid slot allocation"
                        (name class) (name slot) (allocation slot) )) ) )

      (let-values (((can-init value) (compute-init-value slot initargs)))
        (if can-init (init-slot! slot object can-init value)
            (when (init-required? slot)
              (error #f "no init value provided for a required slot"
                     class object slot initargs ) ) ) ) )

    'dummy
) )
