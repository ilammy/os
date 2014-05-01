#!r6rs
(library (os boot internal slot-access)
  ;
  ;   Slot access
  ;
  (export slot-ref-in-<class>
          slot-set-in-<class>!
          slot-bound-in-<class>?)

  (import (except (rnrs base) assert)
          (os meta classes)
          (os internal primitives)
          (os boot meta accessors) )

  (begin

    (define (slot-ref-in-<class> class object slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (effective-slot-direct-getter-ref slot)) )
        (let ((value (slot-ref object)))
          (if (undefined-slot-value? value)
              (error #f "uninitialized slot" object (class-name-ref class) slot-name)
              value ) ) ) )

    (define (slot-set-in-<class>! class object slot-name value)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-set! (effective-slot-direct-setter-ref slot)) )
        (slot-set! object value) ) )

    (define (slot-bound-in-<class>? class object slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (effective-slot-direct-getter-ref slot)) )
        (if (undefined-slot-value? (slot-ref object)) #f #t) ) )

    (define (find-slot-by-name class slot-name)
      (let scan ((slots (class-all-slots-ref class)))
        (cond ((null? slots) (error #f "unknown slot" (class-name-ref class) slot-name))
              ((eq? slot-name (slot-name-ref (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )

) )
