#!r6rs
(library (os boot internal generic-calls)
  ;
  ;   Defining predefined methods
  ;
  (export generic-add-method!
          compute-<method>-function )

  (import (except (rnrs base) assert)
          (rnrs lists)
          (only (srfi :1) first every)
          (only (rnrs sorting) list-sort)
          (os predicates)
          (os internal callables)
          (os internal class-of)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils misc) )

  (begin

    (define (generic-add-method! generic method)
      (assert (callable? generic)
              (eq? <generic> (class-of generic))
              (eq? <method> (class-of method)) )
      (let* ((generic (object-of generic))
             (methods (cons method (generic-methods-ref generic))) )
        (generic-methods-set! generic methods)

        (assert (eq? <linear-method-combinator>
                     (class-of (generic-method-combinator-ref generic)) )
                msg: "Method combinator"
                     (class-name-ref (class-of (generic-method-combinator-ref generic)))
                     "is not expected for predefined generics" )

        (generic-effective-function-set! generic
          (lambda args
            (let* ((arg-classes (map class-of (significant-args generic args)))
                   (applicable-methods (applicable-methods generic arg-classes))
                   (effective-method (effective-method applicable-methods)) )
              (effective-method args) ) ) ) ) )

    (define (significant-args generic args)
      (assert (every (lambda (position) (< position (length args)))
                     (generic-significant-positions-ref generic) )
              msg: "Signatures do not agree.\n"
                   "Signature:" (generic-signature-ref args) "\n"
                   "Significants:" (generic-significant-positions-ref args) "\n"
                   "Args:" args )
      (map (lambda (index) (list-ref args index))
        (generic-significant-positions-ref generic) ) )

    (define (applicable-methods generic arg-classes)
      (let ((all-methods    (generic-methods-ref generic))
            (applicable?    (lambda (method) (method-applicable? method arg-classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs arg-classes))) )
        (list-sort more-specific? (filter applicable? all-methods)) ) )

    (define (method-applicable? method arg-classes)
      (every nonstrict-subclass?
        arg-classes
        (method-discriminators-ref method) ) )

    ; lhs < rhs
    (define (more-specific-method? left-method right-method argument-classes)
      (assert (= (length (method-discriminators-ref left-method))
                 (length (method-discriminators-ref right-method))
                 (length argument-classes) ))
      (let loop ((L (method-discriminators-ref left-method))
                 (R (method-discriminators-ref right-method))
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
                           (class-all-superclasses-ref (car A)) ) ) #t)
              (else (loop (cdr L)
                          (cdr R)
                          (cdr A) )) ) ) )

    ; always linear combinator
    (define (effective-method methods)
      (if (null? methods) (error #f "no applicable methods")
          (let ((methods (map compute-<method>-function methods)))
            (lambda (args)
              ((car methods) (cdr methods) args) ) ) ) )

    (define (compute-<method>-function method)
      (assert (eq? <method> (class-of method)))
      (let ((method-body (method-body-ref method)))
        (lambda (next-methods args)
          (apply method-body
           (if (null? next-methods) #f
               (lambda () ((car next-methods) (cdr next-methods) args)) )
           args ) ) ) )

) )
