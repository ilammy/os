#!r6rs
(library (os boot protocols slot-access)
  ;
  ;   Implementation of protocols/slot-access
  ;
  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class? )

  (import (except (rnrs base) assert)
          (os meta accessors)
          (os internal primitives)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method) )

  (begin

    (predefine-method (slot-ref-in-class $ class object slot-name)
                      `((class ,<class>) object slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (let ((value (slot-ref object)))
          (if (undefined-slot-value? value)
              (error #f "uninitialized slot" object (name class) slot-name)
              value ) ) ) )

    (predefine-method (slot-set-in-class! $ class object slot-name value)
                      `((class ,<class>) object slot-name value)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-set! (direct-setter slot)) )
        (slot-set! object value) ) )

    (predefine-method (slot-bound-in-class? $ class object slot-name)
                      `((class ,<class>) object slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (if (undefined-slot-value? (slot-ref object)) #f #t) ) )

    (define (find-slot-by-name class slot-name)
      (let scan ((slots (all-slots class)))
        (cond ((null? slots) (error #f "unknown slot" (name class) slot-name))
              ((eq? slot-name (name (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )

    'dummy
) )
