(define-library (os boot methods predefine)
  ;
  ;   Defining predefined methods
  ;
  (import (scheme base)
          (only (srfi 1) first filter every)
          (only (srfi 95) sort)
          (os callables)
          (os class-of)
          (os initargs)
          (os primitives)
          (os boot accessors)
          (os boot predicates)
          (os boot classes definitions) )

  (export make-method generic-add-method! predefine-method)

  (begin

    (define <method>-instance-size 2)

    (define (make-method . initargs)
      (let ((method (make-primitive <method> <method>-instance-size)))
        (initialize-method! method initargs)
        method ) )

    (define-syntax predefine-method
      (syntax-rules ()
        ((_ (generic call-next-method . args) (specializers ...) body1 body2 ...)
         (generic-add-method! generic
           (make-method
             'discriminators: (list specializers ...)
             'method-body:
               (lambda (call-next-method . args)
                 body1 body2 ... ) ) ) ) ) )

    (define (initialize-method! method initargs)
      (for-each-initarg
        (lambda (key value)
          (case key
            ((discriminators:) (method-discriminators-set! method value))
            ((method-body:)    (method-body-set!           method value))
            (else (error "unknown init keyword" "<method>" key)) ) )
        initargs ) )

    (define (generic-add-method! generic method)
      (let* ((generic (object-of generic))
             (methods (cons method (generic-methods-ref generic))) )
        (generic-methods-set! generic methods)

        (generic-effective-function-set! generic
          (lambda args
            (let* ((discriminators (map class-of (discriminator-args generic args)))
                   (applicable-methods (applicable-methods generic discriminators))
                   (effective-method (effective-method applicable-methods)) )
              (effective-method args) ) ) ) ) )

    (define (discriminator-args generic args)
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
        (sort (filter applicable? all-methods) more-specific?) ) )

    (define (method-applicable? method classes)
      (every nonstrict-subclass? classes (method-discriminators-ref method)) )

    ; lhs < rhs
    (define (more-specific-method? left-method right-method argument-classes)
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
      (if (null? methods) (error "no applicable methods")
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
