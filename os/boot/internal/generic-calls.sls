#!r6rs
(library (os boot internal generic-calls)
  ;
  ;   Defining predefined methods
  ;
  (export generic-add-method!
          compute-effective-method:<standard>
          safe:compute-effective-function
          safe:find-applicable-methods
          safe:compute-method-function )

  (import (except (rnrs base) assert)
          (rnrs control)
          (rnrs lists)
          (only (srfi :1) first every)
          (only (rnrs sorting) list-sort)
          (os predicates)
          (os meta accessors)
          (os internal callables)
          (os internal class-of)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot internal signature-checks)
          (os protocols generic-calls)
          (os utils assert)
          (os utils misc) )

  (begin

    (define (generic-add-method! generic method)
      (assert (eq? <generic> (class-of generic))
              (eq? <method> (class-of method)) )
      (let ((generic (object-of generic)))
        (assert (signatures-coherent? method generic)
                msg: "Signatures for" (generic-name-ref generic) "are not coherent:"
                     (method-signature-ref method)
                     (generic-signature-ref generic) )
        (assert (eq? <standard-method-combinator>
                     (class-of (generic-method-combinator-ref generic)) )
                msg: "Method combinator"
                     (class-name-ref (class-of (generic-method-combinator-ref generic)))
                     "is not expected for predefined generic function"
                     (generic-name-ref generic) )
        (assert (eq? <method> (generic-method-class-ref generic)))
        (generic-methods-set! generic (cons method (generic-methods-ref generic)))
        (generic-effective-function-set! generic
          (compute-effective-function:<generic> generic) ) ) )

    (define (safe:compute-effective-function generic)
      (if (eq? <generic> (class-of generic))
          (compute-effective-function:<generic> (object-of generic))
          (compute-effective-function generic) ) )

    (define (compute-effective-function:<generic> generic)
      (assert (eq? <generic> (class-of generic)))
      (lambda args
        (let* ((arg-classes (map class-of (significant-args generic args)))
               (applicable-methods (find-applicable-methods:<generic> generic arg-classes))
               (combinator (generic-method-combinator-ref generic))
               (effective-method (safe:compute-effective-method combinator applicable-methods)) )
          (effective-method args) ) ) )

    (define (significant-args generic args)
      (assert (every (lambda (position) (< position (length args)))
                     (generic-significant-positions-ref generic) )
              msg: "Signatures do not agree.\n"
                   "Signature:" (generic-signature-ref args) "\n"
                   "Significants:" (generic-significant-positions-ref args) "\n"
                   "Args:" args )
      (map (lambda (index) (list-ref args index))
        (generic-significant-positions-ref generic) ) )

    (define (safe:find-applicable-methods generic arg-classes)
      (if (eq? <generic> (class-of generic))
          (find-applicable-methods:<generic> (object-of generic) arg-classes)
          (find-applicable-methods generic arg-classes) ) )

    (define (find-applicable-methods:<generic> generic arg-classes)
      (assert (eq? <generic> (class-of generic)))
      (let ((all-methods    (generic-methods-ref generic))
            (applicable?    (lambda (method) (method-applicable? method arg-classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs arg-classes))) )
        (list-sort more-specific? (filter applicable? all-methods)) ) )

    (define (method-applicable? method arg-classes)
      (every nonstrict-subclass? arg-classes (discriminators method)) )

    (define (safe:compute-effective-method combinator applicable-methods)
      (if (eq? <standard-method-combinator> (class-of combinator))
          (compute-effective-method:<standard> applicable-methods)
          (compute-effective-method combinator applicable-methods) ) )

    (define (compute-effective-method:<standard> methods)
      (define (with-qualifier a-qualifier)
        (lambda (method)
          (memq a-qualifier (qualifiers method)) ) )

      (define (without-qualifiers method)
        (null? (qualifiers method)) )

      (let ((arounds   (filter (with-qualifier 'around) methods))
            (befores   (filter (with-qualifier 'before) methods))
            (afters    (filter (with-qualifier 'after)  methods))
            (primaries (filter without-qualifiers       methods)) )
        (if (null? primaries) (error #f "no applicable primary methods")
            (let ((methods (map safe:compute-method-function
                             (append arounds befores primaries
                                     (reverse afters) ) )))
              (lambda (args)
                ((car methods) (cdr methods) args) ) ) ) ) )

    (define (safe:compute-method-function method)
      (if (eq? <method> (class-of method))
          (compute-method-function:<method> method)
          (compute-method-function method) ) )

    (define (compute-method-function:<method> method)
      (assert (eq? <method> (class-of method)))
      (let ((method-body (method-body method)))
        (lambda (next-methods args)
          (apply method-body
           (if (null? next-methods) #f
               (case-lambda
                 (()   ((car next-methods) (cdr next-methods) args))
                 (args ((car next-methods) (cdr next-methods) args)) ) )
           args ) ) ) )

) )
