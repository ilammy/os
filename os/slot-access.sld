(define-library (os slot-access)
  ;
  ;   Slot access
  ;
  (import (scheme base)
          (os class-of)
          (os primitives)
          (os boot class-definitions)
          (os boot accessors) )

  (export slot-ref slot-set! slot-bound?)

  (begin

    (define (slot-ref object slot-name)
      (let ((class (class-of object)))
        (cond
          ((eq? class <class>)          (slot-ref-<class> object slot-name))
          ((eq? class <effective-slot>) (slot-ref-<eslot> object slot-name))
          (else (slot-ref-<object> object slot-name)) ) ) )

    (define (slot-ref-<class> class slot-name)
      (case slot-name
        ((name)                (class-name-ref                class))
        ((direct-superclasses) (class-direct-superclasses-ref class))
        ((direct-slots)        (class-direct-slots-ref        class))
        ((all-superclasses)    (class-all-superclasses-ref    class))
        ((all-slots)           (class-all-slots-ref           class))
        ((instance-size)       (class-instance-size-ref       class))
        (else (error "unknown slot" "<class>" slot-name)) ) )

    (define (slot-ref-<eslot> slot slot-name)
      (case slot-name
        ((name)              (slot-name-ref                    slot))
        ((init-keyword)      (slot-init-keyword-ref            slot))
        ((getter)            (slot-getter-ref                  slot))
        ((setter)            (slot-setter-ref                  slot))
        ((direct-getter)     (effective-slot-direct-getter-ref slot))
        ((direct-setter)     (effective-slot-direct-setter-ref slot))
        (else (error "unknown slot" "<effective-slot>" slot-name)) ) )

    (define (slot-ref-<object> object slot-name)
      (let* ((class (class-of object))
             (slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (let ((value (slot-ref object)))
          (if (undefined-slot-value? value)
              (error "uninitialized slot" (name class) slot-name)
              value ) ) ) )

    (define (slot-set! object slot-name value)
      (let* ((class (class-of object))
             (slot (find-slot-by-name class slot-name))
             (slot-set! (direct-setter slot)) )
        (slot-set! object value) ) )

    (define (slot-bound? object slot-name)
      (let* ((class (class-of object))
             (slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (let ((value (slot-ref object)))
          (if (undefined-slot-value? value) #f #t) ) ) )

    (define (find-slot-by-name class slot-name)
      (let scan ((slots (all-slots class)))
        (cond ((null? slots) (error "unknown slot" (name class) slot-name))
              ((eq? slot-name (name (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )


    ;; All necessary slot accessors have to be defined explicitly here, because
    ;; we can't just say (import (os accessors)) as it causes recursive module
    ;; dependency.

    (define (all-slots o)     (slot-ref o 'all-slots))
    (define (direct-getter o) (slot-ref o 'direct-getter))
    (define (direct-setter o) (slot-ref o 'direct-setter))
    (define (name o)          (slot-ref o 'name))

) )
