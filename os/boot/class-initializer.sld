(define-library (os boot class-initializer)
  ;
  ;   Bootstrapping constructors for classes
  ;
  (import (scheme base)
          (os callables)
          (os primitives)
          (os boot accessors)
          (os boot class-definitions)
          (os boot slot-makers)
          (os boot slot-layout)
          (only (os initargs) for-each-initarg)
          (only (os utils) graph-bfs for-each-with-index) )

  (export initialize-class!)

  (begin

    (define (initialize-class! class . initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
           ((name:)                (class-name-set!                class value))
           ((direct-superclasses:) (class-direct-superclasses-set! class value))
           ((direct-slots:)        (class-direct-slots-set!        class value))
           (else (error "unknown init keyword" "<class>" key)) ) )
        initargs )

      (class-all-superclasses-set! class
        (cdr (graph-bfs class class-direct-superclasses-ref eq?)) )

      (let ((all-slots
              (map (lambda (slot)
                     (make-effective-slot
                       'name:          (slot-name-ref         slot)
                       'init-keyword:  (slot-init-keyword-ref slot)
                       'getter:        (slot-getter-ref       slot)
                       'setter:        (slot-setter-ref       slot) ) )
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

) )
