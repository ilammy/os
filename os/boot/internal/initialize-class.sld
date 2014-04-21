(define-library (os boot internal initialize-class)
  ;
  ;   Bootstrapping constructors for classes
  ;
  (import (scheme base)
          (os internal callables)
          (os internal class-of)
          (os internal primitives)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot internal make-slots)
          (os boot internal slot-layout)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

  (export initialize-class!)

  (begin

    (define (initialize-class! class . initargs)
      (assert (eq? <class> (class-of class)))
      (for-each-initarg
        (lambda (key value)
          (case key
           ((name:)                (class-name-set!                class value))
           ((direct-superclasses:) (class-direct-superclasses-set! class value))
           ((direct-slots:)        (class-direct-slots-set!        class value))
           (else (error "unknown init keyword" "<class>" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (class-name-ref                class)))
              (not (undefined-slot-value? (class-direct-superclasses-ref class)))
              (not (undefined-slot-value? (class-direct-slots-ref        class))) )

      (class-all-superclasses-set! class
        (cdr (graph-bfs class class-direct-superclasses-ref eq?)) )

      (let ((all-slots
              (map (lambda (slot)
                     (make-effective-slot
                       'name:          (safe-value (slot-name-ref         slot))
                       'init-keyword:  (safe-value (slot-init-keyword-ref slot))
                       'getter:        (safe-value (slot-getter-ref       slot))
                       'setter:        (safe-value (slot-setter-ref       slot)) ) )
                (all-slots-of class) ) ))

        (let ((make-getter
               (if (eq? class <generic>)
                   (lambda (index) (lambda (o) (primitive-ref (object-of o) index)))
                   (lambda (index) (lambda (o) (primitive-ref o index))) ) )
              (make-setter
               (if (eq? class <generic>)
                   (lambda (index) (lambda (o v) (primitive-set! (object-of o) index v)))
                   (lambda (index) (lambda (o v) (primitive-set! o index v))) ) ) )

          (for-each-with-index
            (lambda (index slot)
              (effective-slot-direct-getter-set! slot (make-getter index))
              (effective-slot-direct-setter-set! slot (make-setter index)) )
            all-slots ) )

        (class-all-slots-set!     class all-slots)
        (class-instance-size-set! class (length all-slots)) ) )

    (define (safe-value value)
      (if (undefined-slot-value? value) #f value) )

) )
