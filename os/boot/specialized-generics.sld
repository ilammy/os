(define-library (os boot specialized-generics)

  (import (scheme base)
          (only (srfi 1) first find filter every list-index)
          (srfi 2)  ; and-let*
          (srfi 69) ; hash-tables
          (only (srfi 95) sort)
          (os accessors)
          (os callables)
          (os class-of)
          (os initargs)
          (os predicates)
          (os primitives)
          (os slot-access)
          (os utils)
          (os boot accessors)
          (os boot classes definitions)
          (os boot generics definitions)
          (os boot methods predefine) )

  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class?

          compute-all-superclasses
          compute-all-slots
          compute-effective-slot
          compute-instance-size
          finalize-slot-descriptors!
          compute-direct-slot-accessors )

  (begin

    ; protocols/instantiation
    ;
    (predefine-method (make $ class initargs) (<class>)
      (initialize (allocate class initargs) initargs) )

    (predefine-method (allocate $ class initargs) (<class>)
      (make-primitive class (instance-size class)) )

    (predefine-method (initialize $ object initargs) (<object>)
      (for-each
        (lambda (slot)
          (init-slot-with-initargs! slot object initargs) )
        (all-slots (class-of object)) )
      object )

    (define (init-slot-with-initargs! slot object initargs)
      (and-let* ((keyword (init-keyword slot)))
        (let-values (((keyword-found? value) (get-initarg keyword initargs)))
          (when keyword-found?
            (let ((slot-set! (direct-setter slot)))
              (slot-set! object value) ) ) ) ) )

    ; other metaobject initialization
    ;
    (predefine-method (initialize call-next-method class initargs) (<class>)
      (call-next-method)

      (set-all-superclasses! class (compute-all-superclasses class))
      (set-all-slots!        class (compute-all-slots class))
      (set-instance-size!    class (compute-instance-size class))
      (finalize-slot-descriptors! class)

      class )

    (predefine-method (initialize call-next-method generic initargs) (<generic>)
      (call-next-method)

      (generic-methods-set! generic '())
      (generic-effective-function-set! generic
        (lambda args
          (error "no applicable method" (generic-name-ref generic) args) ) )

      generic )

    ; protocols/slot-access
    ;
    (predefine-method (slot-ref-in-class $ object class slot-name)
                      (<object> <class>)
      (let ((value (cond
                     ((eq? class <class>)          (slot-ref-in-<class> object slot-name))
                     ((eq? class <effective-slot>) (slot-ref-in-<eslot> object slot-name))
                     (else (slot-ref-in-<object> object class slot-name)) ) ))
        (if (undefined-slot-value? value)
            (error "uninitialized slot" object (name class) slot-name)
            value ) ) )

    (define (slot-ref-in-<class> class slot-name)
      (case slot-name
        ((name)                (class-name-ref                class))
        ((direct-superclasses) (class-direct-superclasses-ref class))
        ((direct-slots)        (class-direct-slots-ref        class))
        ((all-superclasses)    (class-all-superclasses-ref    class))
        ((all-slots)           (class-all-slots-ref           class))
        ((instance-size)       (class-instance-size-ref       class))
        (else (error "unknown slot" "<class>" slot-name)) ) )

    (define (slot-ref-in-<eslot> eslot slot-name)
      (case slot-name
        ((name)             (slot-name-ref                    eslot))
        ((init-keyword)     (slot-init-keyword-ref            eslot))
        ((getter)           (slot-getter-ref                  eslot))
        ((setter)           (slot-setter-ref                  eslot))
        ((direct-getter)    (effective-slot-direct-getter-ref eslot))
        ((direct-setter)    (effective-slot-direct-setter-ref eslot))
        (else (error "unknown slot" "<effective-slot>" slot-name)) ) )

    (define (slot-ref-in-<object> object class slot-name)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (slot-ref object) ) )

    (predefine-method (slot-set-in-class! $ object class slot-name value)
                      (<object> <class>)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-set! (direct-setter slot)) )
        (slot-set! object value) ) )

    (predefine-method (slot-bound-in-class? $ object class slot-name)
                      (<object> <class>)
      (let* ((slot (find-slot-by-name class slot-name))
             (slot-ref (direct-getter slot)) )
        (if (undefined-slot-value? (slot-ref object)) #f #t) ) )

    (define (find-slot-by-name class slot-name)
      (let scan ((slots (all-slots class)))
        (cond ((null? slots) (error "unknown slot" (name class) slot-name))
              ((eq? slot-name (name (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )


    ; protocols/inheritance
    ;
    (predefine-method (compute-all-superclasses $ class) (<class>)
      ; exclude the class itself from the precendence list
      (cdr (graph-bfs class direct-superclasses eq?)) )

    (predefine-method (compute-all-slots $ class) (<class>)
      (map (lambda (slots)
             (compute-effective-slot class slots) )
        (group-slots-by-name class) ) )

    (predefine-method (compute-effective-slot $ class slots) (<class>)
      (make <effective-slot>
        (list 'name:         (name (car slots))
              'init-keyword: (first-bound 'init-keyword slots)
              'getter:       (first-bound 'getter       slots)
              'setter:       (first-bound 'setter       slots) ) ) )

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

    (predefine-method (compute-instance-size $ class) (<class>)
      (length (all-slots class)) )

    (predefine-method (finalize-slot-descriptors! $ class) (<class>)
      (for-each
        (lambda (slot)
          (let-values (((getter setter) (compute-direct-slot-accessors class slot)))
            (set-direct-getter! slot getter)
            (set-direct-setter! slot setter) ) )
        (all-slots class) ) )

    (predefine-method (compute-direct-slot-accessors $ class slot)
                      (<class> <effective-slot>)
      (let ((index (list-index (lambda (x) (eq? x slot)) (all-slots class))))
        (values (lambda (o)   (primitive-ref  o index))
                (lambda (o v) (primitive-set! o index v)) ) ) )

    (predefine-method (compute-direct-slot-accessors $ callable slot)
                      (<procedure> <effective-slot>)
      (let ((index (list-index (lambda (x) (eq? x slot)) (all-slots callable))))
        (values (lambda (o)   (primitive-ref  (object-of o) index))
                (lambda (o v) (primitive-set! (object-of o) index v)) ) ) )


    ; protocols/generic-calls
    ;
    (predefine-method (add-method! $ generic method) (<generic> <method>)
      (set-methods! generic (cons method (methods generic)))
      (set-effective-function! generic (compute-effective-function generic)) )

    (predefine-method (compute-effective-function $ generic) (<generic>)
      (lambda args
        (let* ((discriminators (map class-of (discriminator-args generic args)))
               (applicable-methods (find-applicable-methods generic discriminators))
               (combinator (method-combinator generic))
               (effective-method (compute-effective-method combinator applicable-methods)) )
          (effective-method args) ) ) )

    (define (discriminator-args generic args)
      (let loop ((result '())
                 (signature (signature generic))
                 (args args) )
        (cond ((null? signature) (reverse result))
              ((pair? (car signature))
               (loop (cons (car args) result)
                     (cdr signature)
                     (cdr args) ) )
              (else (loop result
                          (cdr signature)
                          (cdr args) )) ) ) )

    (predefine-method (find-applicable-methods $ generic classes) (<generic>)
      (let ((all-methods    (methods generic))
            (applicable?    (lambda (method) (method-applicable? method classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs classes))) )
        (sort (filter applicable? all-methods) more-specific?) ) )

    (define (method-applicable? method classes)
      (every instance-of? classes (discriminators method)) )

    ; lhs < rhs
    (predefine-method (more-specific-method? $ left-m right-m arg-classes)
                      (<method> <method>)
      (let loop ((L (discriminators left-m))
                 (R (discriminators right-m))
                 (A arg-classes) )
        (cond ((null? L) #f)
              ((eq? (car L) (car R))
               (loop (cdr L)
                     (cdr R)
                     (cdr A) ) )
              ((subclass? (car L)
                          (car R) ) #t)
              ((memq (car R)
                     (memq (car L)
                           (all-superclasses (car A)) ) ) #t)
              (else (loop (cdr L)
                          (cdr R)
                          (cdr A) )) ) ) )

    (predefine-method (compute-effective-method $ combinator methods)
                      (<linear-method-combinator>)
      (if (null? methods) (error "no applicable methods")
          (let ((methods (map compute-method-function methods)))
            (lambda (args)
              ((car methods) (cdr methods) args) ) ) ) )

    (predefine-method (compute-method-function $ method) (<method>)
      (let ((method-body (method-body method)))
        (lambda (next-methods args)
          (apply method-body
           (if (null? next-methods) #f
               (lambda () ((car next-methods) (cdr next-methods) args)) )
           args ) ) ) )

) )
