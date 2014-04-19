(define-library (os methods)
  ;
  ;   Generic maintenance
  ;
  (import (scheme base)
          (only (srfi 1) first filter every)
          (only (srfi 95) sort)
          (os accessors)
          (os class-of)
          (os predicates) )

  (export add-method!)

  (begin

    (define (add-method! generic method)
      (let ((methods (cons method (methods generic))))
        (set-methods! generic methods)

        (set-effective-function! generic
          (lambda args
            (let* ((discriminators (map class-of (discriminator-args generic args)))
                   (applicable-methods (applicable-methods generic discriminators)) )
              (if (null? applicable-methods)
                  (error "no applicable methods" (name generic) args)
                  (apply (method-body (first applicable-methods)) args) ) ) ) ) ) )

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

    (define (applicable-methods generic classes)
      (let ((all-methods    (methods generic))
            (applicable?    (lambda (method) (method-applicable? method classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs classes))) )
        (sort (filter applicable? all-methods) more-specific?) ) )

    (define (method-applicable? method classes)
      (every nonstrict-subclass? classes (discriminators method)) )

    ; lhs < rhs
    (define (more-specific-method? left-method right-method argument-classes)
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
) )
