#!r6rs
(library (os boot protocols slot-allocation)
  ;
  ;   Implementation of protocols/inheritance
  ;
  (export install-direct-accessors!
          compute-instance-size )

  (import (except (rnrs base) assert)
          (rnrs control)
          (only (srfi :1) any count every find filter partition)
          (os predicates)
          (os meta accessors)
          (os internal callables)
          (os internal class-of)
          (os internal primitives)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-generic)
          (os boot macros predefine-method)
          (os utils assert)
          (os utils misc) )

  (begin

    (define (slots-of an-allocation)
      (lambda (slot)
        (eq? an-allocation (allocation slot)) ) )

    (predefine-method (install-direct-accessors! $ class all-slots) `((class ,<class>) all-slots)
      ;; Personal instance slots
      ;;
      (let ((instance-slots (filter (slots-of 'instance) all-slots)))
        (install-instance-storage-accessors! class instance-slots) )

      ;; Slots belonging to a single class of objects
      ;;
      (let* ((class-slots   (filter (slots-of 'each-subclass) all-slots))
             (class-storage (make-uninitialized-vector (length class-slots))) )
        (install-class-storage-accessors! class class-storage class-slots) )

      ;; Slots belonging to a whole class lineage
      ;;
      (let*-values (((direct inherited) (filter-direct (slots-of 'class-lineage) all-slots class))
                    ((shared-class-storage) (make-uninitialized-vector (length direct))) )
        (install-shared-storage-accessors! class shared-class-storage direct)
        (inherit-direct-slot-accessors! class inherited) )

      ;; Virtual slots without any backing storage
      ;;
      (let-values (((direct inherited) (filter-direct (slots-of 'virtual) all-slots class)))
        (install-virtual-slot-accessors! class direct)
        (inherit-direct-slot-accessors! class inherited) ) )

    (define (filter-direct slot-type all-slots class)
      (let ((class-direct-slots (direct-slots class)))
        (define (slot-with-name a-name)
          (lambda (slot)
            (eq? a-name (name slot)) ) )

        (define (direct-slot? slot)
          (any (slot-with-name (name slot))
            class-direct-slots ) )

        (partition direct-slot?
          (filter slot-type all-slots) ) ) )

    (predefine-method (compute-instance-size $ class) `((class ,<class>))
      (count (slots-of 'instance) (all-slots class)) )

    (predefine-generic compute-direct-instance-accessors `((class ,<class>) slot-index))

    (define (install-instance-storage-accessors! class slots)
      (for-each-with-index
        (lambda (index slot)
          (let-values (((getter setter) (compute-direct-instance-accessors class index)))
            (set-direct-getter! slot getter)
            (set-direct-setter! slot setter) ) )
        slots ) )

    (define (install-class-storage-accessors! class storage slots)
      (for-each-with-index
        (lambda (index slot)
          (let-values (((getter setter) (compute-direct-class-accessors class storage index)))
            (set-direct-getter! slot getter)
            (set-direct-setter! slot setter) ) )
        slots ) )

    (define (install-shared-storage-accessors! class storage slots)
      (for-each-with-index
        (lambda (index slot)
          (let-values (((getter setter) (compute-shared-class-accessors class storage index)))
            (set-direct-getter! slot getter)
            (set-direct-setter! slot setter) ) )
        slots ) )

    (define (install-virtual-slot-accessors! class slots)
      (for-each
        (lambda (slot)
          (predefine-generic get `((object ,class)))
          (predefine-generic set `((object ,class) value))

          (set-direct-getter! slot get)
          (set-direct-setter! slot set) )
        slots ) )

    (define (inherit-direct-slot-accessors! class slots)
      (for-each
        (lambda (inherited-slot)
          (define (original-slot slot)
            (if (eq? (name slot) (name inherited-slot))
                slot #f ) )

          (let scan ((supers (direct-superclasses class)))
            (assert (not (null? supers))
                    msg: "missing an inherited slot" class inherited-slot )
            (let ((found-slot (find original-slot (all-slots (car supers)))))
              (if found-slot
                  (begin (set-direct-getter! inherited-slot (direct-getter found-slot))
                         (set-direct-setter! inherited-slot (direct-setter found-slot)) )
                  (scan (cdr supers)) ) ) ) )
        slots ) )

    (define-syntax class-check
      (syntax-rules ()
        ((_ expected-class object)
         (assert (eq? expected-class (class-of object))
                 msg: "Invalid object class in direct accessor:" (class-of object)
                      "expected:" expected-class ) ) ) )

    (define-syntax subclass-check
      (syntax-rules ()
        ((_ expected-class object)
         (assert (instance-of? expected-class object)
                 msg: "Invalid object class in direct accessor:" (class-of object)
                      "expected:" expected-class ) ) ) )

    (predefine-method (compute-direct-instance-accessors $ class slot-index)
                      `((class ,<class>) slot-index)
      (values (lambda (o)   (class-check class o) (primitive-ref  o slot-index))
              (lambda (o v) (class-check class o) (primitive-set! o slot-index v)) ) )

    (predefine-method (compute-direct-instance-accessors $ callable slot-index)
                      `((callable ,<procedure>) slot-index)
      (values (lambda (o)   (class-check callable (object-of o))
                            (primitive-ref  (object-of o) slot-index) )
              (lambda (o v) (class-check callable (object-of o))
                            (primitive-set! (object-of o) slot-index v) ) ) )

    (define (compute-direct-class-accessors class storage index)
      (values (lambda (o)   (class-check class o) (vector-ref  storage index))
              (lambda (o v) (class-check class o) (vector-set! storage index v)) ) )

    (define (compute-shared-class-accessors class storage index)
      (values (lambda (o)   (subclass-check class o) (vector-ref  storage index))
              (lambda (o v) (subclass-check class o) (vector-set! storage index v)) ) )

    'dummy
) )
