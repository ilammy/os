#!r6rs
(library (os boot protocols inheritance)
  ;
  ;   Implementation of protocols/inheritance
  ;
  (export initialize
            compute-all-superclasses
            compute-all-slots
              compute-effective-slot
              install-direct-accessors!
            compute-instance-size )

  (import (except (rnrs base) assert)
          (rnrs control)
          (compatibility mlist)
          (only (srfi :1) any count every find filter partition)
          (srfi :69) ; hash-tables
          (os predicates)
          (os meta accessors)
          (os internal callables)
          (os internal class-of)
          (os internal primitives)
          (os internal slot-access)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-generic)
          (os boot macros predefine-method)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

  (begin

    (predefine-method (initialize call-next-method class initargs) `((class ,<class>) initargs)
      (call-next-method)

      (set-all-superclasses! class (compute-all-superclasses class))
      (set-all-slots!        class (compute-all-slots class))
      (set-instance-size!    class (compute-instance-size class))

      class )

    (predefine-method (compute-all-superclasses $ class) `((class ,<class>))
      ; exclude the class itself from the precendence list
      (cdr (graph-bfs class direct-superclasses eq?)) )

    (predefine-method (compute-all-slots $ class) `((class ,<class>))
      (let ((all-slots (map (lambda (group)
                              (compute-effective-slot class group) )
                         (group-slots-by-name class) )))
        (install-direct-accessors! class all-slots)
        all-slots ) )

    (predefine-method (compute-effective-slot $ class slots) `((class ,<class>) slots)
      (assert (not (null? slots)))
      (assert (every (lambda (slot) (instance-of? <slot> slot)) slots))
      (apply make <effective-slot>
        (append (list 'name: (name (car slots)))
                (allocation-spec slots)
                (first-specified-in slots 'init-keyword: 'init-keyword)
                (first-specified-in slots 'setter:       'setter)
                (first-specified-in slots 'getter:       'getter)
                (initialization-spec slots) ) ) )

    ;; allocation's default value is `default` which means 'inherit allocation
    ;; from the homonymous superclass slot'. If there are no superclasses with
    ;; such slot then it is treated as `instance`. Also, slot allocation cannot
    ;; be overriden in subclasses, so we check for this when we have a slot with
    ;; non-default allocation present.
    (define (allocation-spec slots)
      (define (get-allocation slots)
        (let scan ((slots slots))
          (if (null? slots) '()
              (begin
                (assert (slot-bound? (car slots) 'allocation))
                (let ((allocation (allocation (car slots))))
                  (if (eq? 'default allocation) (scan (cdr slots))
                      (let ((inherited-allocation (get-allocation (cdr slots))))
                        (if (or (null? inherited-allocation)
                                (eq? allocation inherited-allocation) )
                            allocation
                            (error #f "cannot override allocation of an inherited slot"
                                   (name (car slots))
                                   allocation inherited-allocation ) ) ) ) ) ) ) ) )
      (let ((allocation (get-allocation slots)))
        (list 'allocation: (if (null? allocation) 'instance allocation)) ) )

    ;; init-keyword, getter, and setter slots share the default value: it is #f.
    ;; We are interested in the first explicitly given value in these slots.
    (define (first-specified-in objects init-keyword slot-name)
      (let scan ((objects objects))
        (if (null? objects) '()
            (begin
              (assert (slot-bound? (car objects) slot-name))
              (let ((value (slot-ref (car objects) slot-name)))
                (if value (list init-keyword value)
                          (scan (cdr objects)) ) ) ) ) ) )

    ;; Slot is default-initialized with either a value or a thunk, whichever
    ;; is defined in the latest (first-met) slot specification. However, an
    ;; explicit initialization demand has top priority.
    (define (initialization-spec slots)
      (let scan ((slots slots))
        (cond ((null? slots) '())
              ((init-required? (car slots))          '(init-required: #t))
              ((slot-bound? (car slots) 'init-value) (list 'init-value: (init-value (car slots))))
              ((slot-bound? (car slots) 'init-thunk) (list 'init-thunk: (init-thunk (car slots))))
              (else (scan (cdr slots))) ) ) )

    (define (group-slots-by-name class)
      (let ((hash (make-hash-table eq?)))
        (for-each
          (lambda (direct-slot)
            (let ((slot-name (name direct-slot)))
              (if (hash-table-exists? hash slot-name)
                  (error #f "duplicate direct slot" (name class) slot-name)
                  (hash-table-set! hash slot-name (list direct-slot)) ) ) )
          (direct-slots class) )

        (for-each
          (lambda (superclass)
            (for-each
              (lambda (inherited-slot)
                (hash-table-update! hash (name inherited-slot)
                  (lambda (slots) (cons inherited-slot slots))
                  (lambda () (list inherited-slot)) ) )
              (direct-slots superclass) ) )
          (all-superclasses class) )

        (map reverse (list->mlist (hash-table-values hash))) ) )

    (define (slots-of an-allocation)
      (lambda (slot) (eq? an-allocation (allocation slot))) )

    (predefine-method (compute-instance-size $ class) `((class ,<class>))
      (count (slots-of 'instance) (all-slots class)) )

    (predefine-method (install-direct-accessors! $ class all-slots)
                      `((class ,<class>) all-slots)
      ;; Per instance slots
      (let ((instance-slots (filter (slots-of 'instance) all-slots)))
        (install-instance-storage-accessors! class instance-slots) )

      ;; Per class metaobject slots
      (let ((per-class-slots (filter (slots-of 'each-subclass) all-slots)))
        (unless (null? per-class-slots)
          (let ((per-class-storage (make-uninitialized-vector (length per-class-slots))))
            (install-class-storage-accessors! class per-class-storage per-class-slots) ) ) )

      ;; Per lineage slots
      (let ((shared-class-slots (filter (slots-of 'class) all-slots)))
        (let-values (((direct-slots inherited-slots)
                      (partition-direct-slots class shared-class-slots) ))
          (unless (null? direct-slots)
            (let ((shared-class-storage (make-uninitialized-vector (length direct-slots))))
              (install-shared-storage-accessors! class shared-class-storage direct-slots) ) )

          (inherit-direct-slot-accessors! class inherited-slots) ) )

      (let ((virtual-slots (filter (slots-of 'virtual) all-slots)))
        (let-values (((direct-slots inherited-slots)
                      (partition-direct-slots class virtual-slots) ))
          (install-virtual-slot-accessors! class direct-slots)
          (inherit-direct-slot-accessors! class inherited-slots) ) ) )

    (define (partition-direct-slots class slots)
      (define class-direct-slots (direct-slots class))

      (define (slot-with-name a-name)
        (lambda (slot)
          (eq? a-name (name slot)) ) )

      (define (direct-slot? slot)
        (any (slot-with-name (name slot))
          class-direct-slots ) )

      (partition direct-slot? slots) )

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

    (define (inherit-direct-slot-accessors! class slots)
      (for-each
        (lambda (goal-slot)
          (define (original-slot test-slot)
            (if (eq? (name test-slot) (name goal-slot))
                test-slot
                #f ) )
          (let scan ((supers (direct-superclasses class)))
            (assert (not (null? supers)) msg: "Missing inherited slot")
            (let ((found-slot (find original-slot (all-slots (car supers)))))
              (if found-slot
                  (begin (set-direct-getter! goal-slot (direct-getter found-slot))
                         (set-direct-setter! goal-slot (direct-setter found-slot)) )
                  (scan (cdr supers)) ) ) ) )
        slots ) )

    (define (install-virtual-slot-accessors! class slots)
      (for-each
        (lambda (slot)
          (predefine-generic get `((object ,class)))
          (predefine-generic set `((object ,class) value))
          (set-direct-getter! slot get)
          (set-direct-setter! slot set) )
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

    (define-syntax checked-instance-ref
      (syntax-rules ()
        ((_ expected-class object index)
         (begin (assert (primitive? object))
                (class-check expected-class object)
                (primitive-ref object index) ) ) ) )

    (define-syntax checked-instance-set!
      (syntax-rules ()
        ((_ expected-class object index value)
         (begin (assert (primitive? object))
                (class-check expected-class object)
                (primitive-set! object index value) ) ) ) )

    (predefine-method (compute-direct-instance-accessors $ class slot-index)
                      `((class ,<class>) slot-index)
      (values (lambda (o)   (checked-instance-ref  class o slot-index))
              (lambda (o v) (checked-instance-set! class o slot-index v)) ) )

    (predefine-method (compute-direct-instance-accessors $ callable slot-index)
                      `((callable ,<procedure>) slot-index)
      (values (lambda (o)   (checked-instance-ref  callable (object-of o) slot-index))
              (lambda (o v) (checked-instance-set! callable (object-of o) slot-index v)) ) )

    (define-syntax checked-storage-ref
      (syntax-rules ()
        ((_ expected-class object storage index)
         (begin (class-check expected-class object)
                (vector-ref storage index) ) ) ) )

    (define-syntax checked-storage-set!
      (syntax-rules ()
        ((_ expected-class object storage index value)
         (begin (class-check expected-class object)
                (vector-set! storage index value) ) ) ) )

    (define (compute-direct-class-accessors class storage index)
      (values (lambda (o)   (checked-storage-ref  class o storage index))
              (lambda (o v) (checked-storage-set! class o storage index v)) ) )

    (define-syntax checked-shared-storage-ref
      (syntax-rules ()
        ((_ expected-class object storage index)
         (begin (subclass-check expected-class object)
                (vector-ref storage index) ) ) ) )

    (define-syntax checked-shared-storage-set!
      (syntax-rules ()
        ((_ expected-class object storage index value)
         (begin (subclass-check expected-class object)
                (vector-set! storage index value) ) ) ) )

    (define (compute-shared-class-accessors class storage index)
      (values (lambda (o)   (checked-shared-storage-ref  class o storage index))
              (lambda (o v) (checked-shared-storage-set! class o storage index v)) ) )

    'dummy
) )
