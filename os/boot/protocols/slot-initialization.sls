#!r6rs
(library (os boot protocols slot-initialization)
  ;
  ;   Implementation of protocols/inheritance
  ;
  (export compute-effective-slot)

  (import (except (rnrs base) assert)
          (only (srfi :1) every)
          (os predicates)
          (os meta accessors)
          (os internal slot-access)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils assert)
          (os utils initargs)
          (os utils misc) )

  (begin

    (predefine-method (compute-effective-slot $ class slots) `((class ,<class>) slots)
      (assert (not (null? slots)))
      (assert (every (lambda (slot) (instance-of? <slot> slot)) slots))
      (apply make <effective-slot>
        (append (list 'name: (name (car slots)))
                (compute-effective-allocation-spec     slots)
                (compute-effective-slot-initarg-spec   slots 'init-keyword: 'init-keyword)
                (compute-effective-slot-initarg-spec   slots 'setter:       'setter)
                (compute-effective-slot-initarg-spec   slots 'getter:       'getter)
                (compute-effective-initialization-spec slots) ) ) )

    ;; allocation's default value is `default` which means 'inherit allocation
    ;; from the homonymous superclass slot'. If there are no superclasses with
    ;; such slot then it is treated as `instance`. Also, slot allocation cannot
    ;; be overriden in subclasses, so we check for this when we have a slot with
    ;; non-default allocation present.
    (define (compute-effective-allocation-spec slots)
      (define (get-allocation slots)
        (let ((allocation (scan-allocations slots)))
          (if allocation allocation 'instance) ) )

      (define (scan-allocations slots)
        (if (null? slots) #f
            (skip-default-allocations slots) ) )

      (define (skip-default-allocations slots)
        (assert (slot-bound? (car slots) 'allocation))
        (let ((allocation (allocation (car slots))))
          (if (eq? 'default allocation)
              (scan-allocations (cdr slots))
              (check-for-valid-allocation allocation slots) ) ) )

      (define (check-for-valid-allocation allocation slots)
        (let ((inherited-allocation (scan-allocations (cdr slots))))
          (if (or (not inherited-allocation)
                  (eq? allocation inherited-allocation) )
              allocation
              (error #f "cannot override allocation of an inherited slot"
                     (name (car slots))
                     allocation inherited-allocation ) ) ) )

      (list 'allocation: (get-allocation slots)) )

    ;; init-keyword, getter, and setter slots share the default value: it is #f.
    ;; We are interested in the first explicitly given value in these slots.
    (define (compute-effective-slot-initarg-spec slots init-keyword slot-name)
      (let scan ((slots slots))
        (if (null? slots) '()
            (begin
              (assert (slot-bound? (car slots) slot-name))
              (let ((value (slot-ref (car slots) slot-name)))
                (if value (list init-keyword value)
                          (scan (cdr slots)) ) ) ) ) ) )

    ;; Slot is default-initialized with either a value or a thunk, whichever
    ;; is defined in the latest (first-met) slot specification. However, an
    ;; explicit initialization demand has top priority.
    (define (compute-effective-initialization-spec slots)
      (let scan ((slots slots))
        (cond ((null? slots) '())
              ((init-required? (car slots))         '(init-required: #t))
              ((slot-bound? (car slots) 'init-value) (list 'init-value: (init-value (car slots))))
              ((slot-bound? (car slots) 'init-thunk) (list 'init-thunk: (init-thunk (car slots))))
              (else (scan (cdr slots))) ) ) )

    'dummy
) )
