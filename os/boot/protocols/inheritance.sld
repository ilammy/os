(define-library (os boot protocols inheritance)
  ;
  ;   Implementation of protocols/inheritance
  ;
  (import (scheme base)
          (only (srfi 1) find list-index)
          (srfi 69) ; hash-tables
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

  (export initialize
            compute-all-superclasses
            compute-all-slots
              compute-effective-slot
            compute-instance-size
            finalize-slot-descriptors!
              compute-direct-slot-accessors )

  (begin

    (predefine-method (initialize call-next-method class initargs) (<class>)
      (call-next-method)

      (set-all-superclasses! class (compute-all-superclasses class))
      (set-all-slots!        class (compute-all-slots class))
      (set-instance-size!    class (compute-instance-size class))
      (finalize-slot-descriptors! class)

      class )

    (predefine-method (compute-all-superclasses $ class) (<class>)
      ; exclude the class itself from the precendence list
      (cdr (graph-bfs class direct-superclasses eq?)) )

    (predefine-method (compute-all-slots $ class) (<class>)
      (map (lambda (slots)
             (compute-effective-slot class slots) )
        (group-slots-by-name class) ) )

    (predefine-method (compute-effective-slot $ class slots) (<class>)
      (apply make <effective-slot>
        (construct-first-bound-initargs slots
          'name:          'name
          'init-keyword:  'init-keyword
          'init-required: 'init-required
          'init-value:    'init-value
          'init-thunk:    'init-thunk
          'getter:        'getter
          'setter:        'setter ) ) )

    (define (construct-first-bound-initargs slots . initarg-template)
      (let ((result '()))
        (for-each-initarg
          (lambda (key slot-name)
            (let-values (((found? value) (find-bound slot-name slots)))
              (when found?
                (set! result (cons value (cons key result))) ) ) )
          initarg-template )

        (reverse result) ) )

    (define (find-bound slot-name objects)
      (let ((slot-bound? (lambda (object) (slot-bound? object slot-name))))
        (let ((object (find slot-bound? objects)))
          (if object
              (values #t (slot-ref object slot-name))
              (values #f #f) ) ) ) )

    (define (group-slots-by-name class)
      (let ((hash (make-hash-table eq?)))
        (for-each
          (lambda (direct-slot)
            (let ((slot-name (name direct-slot)))
              (if (hash-table-exists? hash slot-name)
                  (error "duplicate direct slot" (name class) slot-name)
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

        (map reverse (hash-table-values hash)) ) )

    (predefine-method (compute-instance-size $ class) (<class>)
      (length (all-slots class)) )

    (predefine-method (finalize-slot-descriptors! $ class) (<class>)
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
                      (<class> <effective-slot>)
      (let ((index (list-index (lambda (x) (eq? x slot)) (all-slots class))))
        (values (lambda (o)   (checked-ref  class o index))
                (lambda (o v) (checked-set! class o index v)) ) ) )

    (predefine-method (compute-direct-slot-accessors $ callable slot)
                      (<procedure> <effective-slot>)
      (let ((index (list-index (lambda (x) (eq? x slot)) (all-slots callable))))
        (values (lambda (o)   (checked-ref  callable (object-of o) index))
                (lambda (o v) (checked-set! callable (object-of o) index v)) ) ) )

) )
