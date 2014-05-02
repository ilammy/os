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
          (compatibility mlist)
          (only (srfi :1) count every find filter list-index)
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
      (let* ((slot-groups (group-slots-by-name class))
             (all-slots (map (lambda (group) (compute-effective-slot class group))
                             slot-groups )) )
        (install-direct-accessors! class all-slots slot-groups)
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

    (predefine-generic compute-direct-instance-accessors `((class ,<class>) slot-index))

    (predefine-method (install-direct-accessors! $ class all-slots slot-groups)
                      `((class ,<class>) all-slots slot-groups)
      (assert (= (length all-slots) (length slot-groups)))
      (let ((instance-slots (filter (slots-of 'instance) all-slots))
            (per-class-slots (filter (slots-of 'each-subclass) all-slots)) )
        (for-each-with-index
          (lambda (index slot)
            (let-values (((getter setter) (compute-direct-instance-accessors class index)))
              (set-direct-getter! slot getter)
              (set-direct-setter! slot setter) ) )
          instance-slots )

        (let ((per-class-storage (make-uninitialized-vector (length per-class-slots))))
          (for-each-with-index
            (lambda (index slot)
              (let-values (((getter setter) (compute-direct-per-class-accessors
                                              class per-class-storage index )))
                (set-direct-getter! slot getter)
                (set-direct-setter! slot setter) ) )
            per-class-slots ) ) ) )

    (define-syntax class-check
      (syntax-rules ()
        ((_ expected-class object)
         (assert (eq? expected-class (class-of object))
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

    (define (compute-direct-per-class-accessors class storage index)
      (values (lambda (o)   (checked-storage-ref  class o storage index))
              (lambda (o v) (checked-storage-set! class o storage index v)) ) )

    'dummy
) )
