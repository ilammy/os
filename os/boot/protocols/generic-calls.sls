#!r6rs
(library (os boot protocols generic-calls)
  ;
  ;   Implementation of protocols/generic-calls
  ;
  (export add-method!
            compute-effective-function
              find-applicable-methods
                more-specific-method?
              compute-effective-method
                compute-method-function )

  (import (except (rnrs base) assert)
          (rnrs lists)
          (only (srfi :1) every)
          (only (rnrs sorting) list-sort)
          (os predicates)
          (os meta accessors)
          (os internal class-of)
          (os boot internal generic-calls)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils assert)
          (os utils misc) )

  (begin

    (predefine-method (add-method! $ generic method) `((generic ,<generic>) (method ,<method>))
      (set-methods! generic (cons method (methods generic)))
      (set-effective-function! generic (compute-effective-function generic)) )

    (predefine-method (compute-effective-function $ generic) `((generic ,<generic>))
      (lambda args
        (let* ((arg-classes (map class-of (significant-args generic args)))
               (applicable-methods (find-applicable-methods generic arg-classes))
               (combinator (method-combinator generic))
               (effective-method (compute-effective-method combinator applicable-methods)) )
          (effective-method args) ) ) )

    (define (significant-args generic args)
      (assert (every (lambda (position) (< position (length args)))
                     (significant-positions generic) )
              msg: "Signatures do not agree.\n"
                   "Signature:" (signature args) "\n"
                   "Significants:" (significant-positions args) "\n"
                   "Args:" args )
      (map (lambda (index) (list-ref args index))
        (significant-positions generic) ) )

    (predefine-method (find-applicable-methods $ generic arg-classes) `((generic ,<generic>) arg-classes)
      (let ((all-methods    (methods generic))
            (applicable?    (lambda (method) (method-applicable? method arg-classes)))
            (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs arg-classes))) )
        (list-sort more-specific? (filter applicable? all-methods)) ) )

    (define (method-applicable? method arg-classes)
      (every nonstrict-subclass? arg-classes (discriminators method)) )

    ; lhs < rhs
    (predefine-method (more-specific-method? $ left-m right-m arg-classes)
                      `((left-m ,<method>) (right-m ,<method>) arg-classes)
      (assert (= (length (discriminators left-m))
                 (length (discriminators right-m))
                 (length arg-classes) ))
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
                      `((combinator ,<linear-method-combinator>) methods)
      (if (null? methods) (error #f "no applicable methods")
          (let ((methods (map (lambda (method)
                                (if (eq? <method> (class-of method))
                                    (compute-<method>-function method)
                                    (compute-method-function   method) ) )
                           methods )))
            (lambda (args)
              ((car methods) (cdr methods) args) ) ) ) )

    (predefine-method (compute-method-function $ method) `((method ,<method>))
      (let ((method-body (method-body method)))
        (lambda (next-methods args)
          (apply method-body
           (if (null? next-methods) #f
               (lambda () ((car next-methods) (cdr next-methods) args)) )
           args ) ) ) )

  'dummy

) )
