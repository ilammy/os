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
          (os internal class-of)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils assert) )

  (begin

    (predefine-method (slot-ref-in-class $ class object slot-name)
                      `((class ,<class>) object slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (get-direct-getter slot)) )
        (let ((value (slot-ref object)))
          (if (undefined-slot-value? value)
              (error #f "uninitialized slot" object (name class) slot-name)
              value ) ) ) )

    (predefine-method (slot-set-in-class! $ class object slot-name value)
                      `((class ,<class>) object slot-name value)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-set! (get-direct-setter slot)) )
        (slot-set! object value) ) )

    (predefine-method (slot-bound-in-class? $ class object slot-name)
                      `((class ,<class>) object slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (get-direct-getter slot)) )
        (if (undefined-slot-value? (slot-ref object)) #f #t) ) )

    (define (get-direct-getter effective-slot)
      (if (eq? <effective-slot> (class-of effective-slot))
          (effective-slot-direct-getter-ref effective-slot)
          (direct-getter effective-slot) ) )

    (define (get-direct-setter effective-slot)
      (if (eq? <effective-slot> (class-of effective-slot))
          (effective-slot-direct-setter-ref effective-slot)
          (direct-setter effective-slot) ) )

    (define (find-slot-by-name class slot-name)
      (if (or (eq? class <class>) (eq? class <effective-slot>))
          (find-slot-by-name:predefined class slot-name)
          (find-slot-by-name:generic class slot-name) ) )

    (define (find-slot-by-name:predefined class slot-name)
      (let scan ((slots (class-all-slots-ref class)))
        (cond ((null? slots) (error #f "unknown slot" (class-name-ref class) slot-name))
              ((eq? slot-name (slot-name-ref (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )

    (define (find-slot-by-name:generic class slot-name)
      (let scan ((slots (all-slots class)))
        (cond ((null? slots) (error #f "unknown slot" (name class) slot-name))
              ((eq? slot-name (name (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )

    'dummy
) )
