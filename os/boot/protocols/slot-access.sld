(define-library (os boot protocols slot-access)
  ;
  ;   Implementation of protocols/slot-access
  ;
  (import (scheme base)
          (os meta accessors)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method) )

  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class? )

  (begin

    (predefine-method (slot-ref-in-class $ object class slot-name)
                      (<object> <class>)
      (let ((value (cond
                     ((eq? class <class>)          (slot-ref-in-<class> object slot-name))
                     ((eq? class <effective-slot>) (slot-ref-in-<eslot> object slot-name))
                     (else (slot-ref-in-<object> object class slot-name)) ) ))
        (if (undefined-slot-value? value)
            (error "uninitialized slot" object (name class) slot-name)
            value ) ) )

    (define (slot-ref-in-<class> class slot-name)
      (case slot-name
        ((name)                (class-name-ref                class))
        ((direct-superclasses) (class-direct-superclasses-ref class))
        ((direct-slots)        (class-direct-slots-ref        class))
        ((all-superclasses)    (class-all-superclasses-ref    class))
        ((all-slots)           (class-all-slots-ref           class))
        ((instance-size)       (class-instance-size-ref       class))
        (else (error "unknown slot" "<class>" slot-name)) ) )

    (define (slot-ref-in-<eslot> eslot slot-name)
      (case slot-name
        ((name)             (slot-name-ref                    eslot))
        ((init-keyword)     (slot-init-keyword-ref            eslot))
        ((getter)           (slot-getter-ref                  eslot))
        ((setter)           (slot-setter-ref                  eslot))
        ((direct-getter)    (effective-slot-direct-getter-ref eslot))
        ((direct-setter)    (effective-slot-direct-setter-ref eslot))
        (else (error "unknown slot" "<effective-slot>" slot-name)) ) )

    (define (slot-ref-in-<object> object class slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (slot-ref object) ) )

    (predefine-method (slot-set-in-class! $ object class slot-name value)
                      (<object> <class>)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-set! (direct-setter slot)) )
        (slot-set! object value) ) )

    (predefine-method (slot-bound-in-class? $ object class slot-name)
                      (<object> <class>)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (if (undefined-slot-value? (slot-ref object)) #f #t) ) )

    (define (find-slot-by-name class slot-name)
      (let scan ((slots (all-slots class)))
        (cond ((null? slots) (error "unknown slot" (name class) slot-name))
              ((eq? slot-name (name (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )

) )
