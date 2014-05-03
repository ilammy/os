#!r6rs
(library (os boot internal generic-calls)
  ;
  ;   Defining predefined methods
  ;
  (export generic-add-method!
          compute-effective-function:<generic>
          find-applicable-methods:<generic>
          compute-method-function:<method> )

  (import (except (rnrs base) assert)
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
          (only (os protocols generic-calls) compute-method-function)
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
        (assert (eq? <method> (generic-method-class-ref generic)))
        (generic-methods-set! generic (cons method (generic-methods-ref generic)))
        (generic-effective-function-set! generic
          (compute-effective-function:<generic> generic) ) ) )

    (define (compute-effective-function:<generic> generic)
      (assert (eq? <generic> (class-of generic)))
      (assert (eq? <linear-method-combinator>
                   (class-of (generic-method-combinator-ref generic)) )
              msg: "Method combinator"
                   (class-name-ref (class-of (generic-method-combinator-ref generic)))
                   "is not expected for predefined generic function"
                   (generic-name-ref generic) )
      (lambda args
        (let* ((arg-classes (map class-of (significant-args generic args)))
               (applicable-methods (find-applicable-methods:<generic> generic arg-classes))
               (effective-method (effective-method applicable-methods)) )
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

    (define (find-applicable-methods:<generic> generic arg-classes)
      (assert (eq? <generic> (class-of generic)))
      (let ((all-methods    (generic-methods-ref generic))
            (applicable?    (lambda (method) (method-applicable? method arg-classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs arg-classes))) )
        (list-sort more-specific? (filter applicable? all-methods)) ) )

    (define (method-applicable? method arg-classes)
      (every nonstrict-subclass?
        arg-classes
        (discriminators method) ) )

    ; lhs < rhs
    (define (more-specific-method? left-method right-method argument-classes)
      (assert (= (length (discriminators left-method))
                 (length (discriminators right-method))
                 (length argument-classes) ))
      (let loop ((L (discriminators left-method))
                 (R (discriminators right-method))
                 (A argument-classes) )
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

    ; always linear combinator
    (define (effective-method methods)
      (if (null? methods) (error #f "no applicable methods")
          (let ((methods (map (lambda (method)
                                (if (eq? <method> (class-of method))
                                    (compute-method-function:<method> method)
                                    (compute-method-function method) ) )
                           methods )))
            (lambda (args)
              ((car methods) (cdr methods) args) ) ) ) )

    (define (compute-method-function:<method> method)
      (assert (eq? <method> (class-of method)))
      (let ((method-body (method-body method)))
        (lambda (next-methods args)
          (apply method-body
           (if (null? next-methods) #f
               (lambda () ((car next-methods) (cdr next-methods) args)) )
           args ) ) ) )

) )
