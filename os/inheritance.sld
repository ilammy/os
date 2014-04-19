(define-library (os inheritance)
  ;
  ;   Class inheritance
  ;
  (import (scheme base)
          (only (srfi 1) find)
          (srfi 2) ; and-let*
          (srfi 69) ; hash-tables
          (os accessors)
          (os callables)
          (os instantiation)
          (os predicates)
          (os primitives)
          (os slot-access)
          (os boot class-definitions)
          (only (os utils) graph-bfs for-each-with-index) )

  (export make-class)

  (begin

    (define (make-class . initargs)
      (let ((class (apply make <class> initargs)))
        (set-all-superclasses! class
          (cdr (graph-bfs class direct-superclasses eq?)) ) ; exclude the class itself

        (let ((all-slots
               (map (lambda (slots)
                      (make <effective-slot>
                        'name:         (name (car slots))
                        'init-keyword: (first-bound 'init-keyword slots)
                        'getter:       (first-bound 'getter       slots)
                        'setter:       (first-bound 'setter       slots) ) )
                 (group-slots-by-name class) ) ))

          (let ((make-getter
                 (if (subclass? class <procedure>)
                     (lambda (index) (lambda (o) (primitive-ref (object-of o) index)))
                     (lambda (index) (lambda (o) (primitive-ref o index))) ) )
                (make-setter
                 (if (subclass? class <procedure>)
                     (lambda (index) (lambda (o v) (primitive-set! (object-of o) index v)))
                     (lambda (index) (lambda (o v) (primitive-set! o index v))) ) ) )

            (for-each-with-index
              (lambda (index slot)
                (set-direct-getter! slot (make-getter index))
                (set-direct-setter! slot (make-setter index)) )
              all-slots ) )

          (set-all-slots!     class all-slots)
          (set-instance-size! class (length all-slots)) )

        class ) )

    (define (first-bound slot-name objects)
      (let ((slot-bound? (lambda (object) (slot-bound? object slot-name))))
        (and-let* ((object (find slot-bound? objects)))
          (slot-ref object slot-name) ) ) )

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

) )
