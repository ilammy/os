#!r6rs
(library (os boot internal generic-calls)
  ;
  ;   Defining predefined methods
  ;
  (export generic-add-method!)

  (import (except (rnrs base) assert)
          (rnrs lists)
          (only (srfi :1) first every)
          (only (rnrs sorting) list-sort)
          (os internal callables)
          (os internal class-of)
          (os boot meta accessors)
          (os boot meta classes)
          (os boot predicates)
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
            (let* ((discriminators (map class-of (discriminator-args generic args)))
                   (applicable-methods (applicable-methods generic discriminators))
                   (effective-method (effective-method applicable-methods)) )
              (effective-method args) ) ) ) ) )

    (define (discriminator-args generic args)
      (assert (<= (proper-length (generic-signature-ref generic))
                  (length args) ))
      (let loop ((result '())
                 (signature (generic-signature-ref generic))
                 (args args) )
        (cond ((or (null? signature)
                   (symbol? signature) ) (reverse result))
              ((pair? (car signature))
               (loop (cons (car args) result)
                     (cdr signature)
                     (cdr args) ) )
              (else (loop result
                          (cdr signature)
                          (cdr args) )) ) ) )

    (define (applicable-methods generic classes)
      (let ((all-methods    (generic-methods-ref generic))
            (applicable?    (lambda (method) (method-applicable? method classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs classes))) )
        (list-sort more-specific? (filter applicable? all-methods)) ) )

    (define (method-applicable? method classes)
      (every nonstrict-subclass? classes (method-discriminators-ref method)) )

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
          (let ((methods (map method-function methods)))
            (lambda (args)
              ((car methods) (cdr methods) args) ) ) ) )

    (define (method-function method)
      (let ((method-body (method-body-ref method)))
        (lambda (next-methods args)
          (apply method-body
           (if (null? next-methods) #f
               (lambda () ((car next-methods) (cdr next-methods) args)) )
           args ) ) ) )

) )
