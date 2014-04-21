(define-library (os boot methods generic-calls)
  ;
  ;   Implementation of protocols/generic-calls
  ;
  (import (scheme base)
          (only (srfi 1) filter every)
          (only (srfi 95) sort)
          (os accessors)
          (os class-of)
          (os predicates)
          (os boot classes definitions)
          (os boot generics definitions)
          (os boot methods predefine) )

  (export add-method!
            compute-effective-function
              find-applicable-methods
                more-specific-method?
              compute-effective-method
                compute-method-function )

  (begin

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
      (every nonstrict-subclass? classes (discriminators method)) )

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
