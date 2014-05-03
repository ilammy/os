#!r6rs
(library (os boot internal initialize-class)
  ;
  ;   Bootstrapping constructors for classes
  ;
  (export initialize-class!)

  (import (except (rnrs base) assert)
          (rnrs control)
          (only (srfi :1) every)
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

  (begin

    (define (initialize-class! class . initargs)
      (assert (eq? <class> (class-of class)))

      (initialize-slots-with-initargs! class initargs)

      (class-all-superclasses-set! class
        (cdr (graph-bfs class class-direct-superclasses-ref eq?)) )

      (let ((all-slots (install-direct-slot-accessors! class
                        (compute-all-class-slots class) )))
        (class-all-slots-set! class all-slots)
        (class-instance-size-set! class (length all-slots)) )

      class )

    (define (initialize-slots-with-initargs! class initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
           ((name:)                (class-name-set!                class value))
           ((direct-superclasses:) (class-direct-superclasses-set! class value))
           ((direct-slots:)        (class-direct-slots-set!        class value))
           ((abstract:)            (class-abstract?-set!           class value))
           (else (assert #f msg: "Unknown init keyword used for <class>:" key)) ) )
        initargs )

      (assert (not (undefined-slot-value? (class-direct-superclasses-ref class)))
              (not (undefined-slot-value? (class-direct-slots-ref        class)))
              msg: "Required slots of a <class> are not initialized" )

      (when (undefined-slot-value? (class-name-ref class))
        (class-name-set! class '<anonymous>) )

      (when (undefined-slot-value? (class-abstract?-ref class))
        (class-abstract?-set! class #f) )

      (assert (if (class-abstract?-ref class)
                  (every class-abstract?-ref
                    (class-direct-superclasses-ref class) )
                  #t )
              msg: "Abstract class cannot have concrete superclasses" ) )

    (define (compute-all-class-slots class)
      (map (lambda (slot)
             (apply make-effective-slot
               (with-defined-initargs
                 'name:          (slot-name-ref          slot)
                 'init-keyword:  (slot-init-keyword-ref  slot)
                 'init-required: (slot-init-required-ref slot)
                 'init-value:    (slot-init-value-ref    slot)
                 'init-thunk:    (slot-init-thunk-ref    slot)
                 'getter:        (slot-getter-ref        slot)
                 'setter:        (slot-setter-ref        slot) ) ) )
        (all-slots-of class) ) )

    (define (install-direct-slot-accessors! class all-slots)
      (define (make-getter index)
        (if (eq? class <generic>)
            (lambda (o) (primitive-ref (object-of o) index))
            (lambda (o) (primitive-ref o index)) ) )

      (define (make-setter index)
        (if (eq? class <generic>)
            (lambda (o v) (primitive-set! (object-of o) index v))
            (lambda (o v) (primitive-set! o index v)) ) )

      (for-each-with-index
        (lambda (index slot)
          (effective-slot-direct-getter-set! slot (make-getter index))
          (effective-slot-direct-setter-set! slot (make-setter index)) )
        all-slots )

      all-slots )

    (define (with-defined-initargs . initargs)
      (let ((result '()))
        (for-each-initarg
          (lambda (key value)
            (unless (undefined-slot-value? value)
              (set! result (cons value (cons key result))) ) )
          initargs )

        (reverse result) ) )

) )
