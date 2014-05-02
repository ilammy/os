#!r6rs
(library (os boot protocols inheritance)
  ;
  ;   Implementation of protocols/inheritance
  ;
  (export initialize
            compute-all-superclasses
            compute-all-slots
              compute-effective-slot
            compute-instance-size
            finalize-slot-descriptors!
              compute-direct-slot-accessors )

  (import (except (rnrs base) assert)
          (compatibility mlist)
          (only (srfi :1) find list-index every)
          (srfi :69) ; hash-tables
          (os predicates)
          (os meta accessors)
          (os internal callables)
          (os internal primitives)
          (os internal slot-access)
          (os boot meta classes)
          (os boot meta generics)
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
      (finalize-slot-descriptors! class)

      class )

    (predefine-method (compute-all-superclasses $ class) `((class ,<class>))
      ; exclude the class itself from the precendence list
      (cdr (graph-bfs class direct-superclasses eq?)) )

    (predefine-method (compute-all-slots $ class) `((class ,<class>))
      (map (lambda (slots)
             (compute-effective-slot class slots) )
        (group-slots-by-name class) ) )

    (predefine-method (compute-effective-slot $ class slots) `((class ,<class>) slots)
      (assert (not (null? slots)))
      (assert (every (lambda (o) (instance-of? <slot> o)) slots))
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

    (predefine-method (compute-instance-size $ class) `((class ,<class>))
      (length (all-slots class)) )

    (predefine-method (finalize-slot-descriptors! $ class) `((class ,<class>))
      (for-each
        (lambda (slot)
          (let-values (((getter setter) (compute-direct-slot-accessors class slot)))
            (set-direct-getter! slot getter)
            (set-direct-setter! slot setter) ) )
        (all-slots class) ) )

    (define-syntax checked-ref
      (syntax-rules ()
        ((_ expected-class object index)
         (begin (assert (primitive? object)
                        (eq? (primitive-class object) expected-class)
                        msg: "Invalid object passed to direct accessor" )
                (primitive-ref object index) ) ) ) )

    (define-syntax checked-set!
      (syntax-rules ()
        ((_ expected-class object index value)
         (begin (assert (primitive? object)
                        (eq? (primitive-class object) expected-class)
                        msg: "Invalid object passed to direct accessor" )
                (primitive-set! object index value) ) ) ) )

    (predefine-method (compute-direct-slot-accessors $ class slot)
                      `((class ,<class>) (slot ,<effective-slot>))
      (let ((index (list-index (lambda (x) (eq? x slot)) (all-slots class))))
        (values (lambda (o)   (checked-ref  class o index))
                (lambda (o v) (checked-set! class o index v)) ) ) )

    (predefine-method (compute-direct-slot-accessors $ callable slot)
                      `((callable ,<procedure>) (slot ,<effective-slot>))
      (let ((index (list-index (lambda (x) (eq? x slot)) (all-slots callable))))
        (values (lambda (o)   (checked-ref  callable (object-of o) index))
                (lambda (o v) (checked-set! callable (object-of o) index v)) ) ) )

    'dummy
) )
