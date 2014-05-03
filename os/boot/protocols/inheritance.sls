#!r6rs
(library (os boot protocols inheritance)
  ;
  ;   Implementation of protocols/inheritance
  ;
  (export initialize ; for <class>
            compute-all-superclasses
            compute-all-slots )

  (import (except (rnrs base) assert)
          (compatibility mlist)
          (srfi :69) ; hash-tables
          (os meta accessors)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
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

    'dummy
) )
