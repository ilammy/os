#!r6rs
(library (os boot internal signature-checks)
  ;
  ;   Class relation predicates
  ;
  (export valid-signature?
          signatures-coherent? )

  (import (except (rnrs base) assert)
          (os boot predicates)
          (os boot meta accessors)
          (os boot meta classes)
          (os utils assert)
          (os utils misc) )

  (begin

    (define (specializer? object) (instance-of? <class> object))

    (define (valid-signature? signature)
      (let loop ((signature signature))
        (cond ((or (null? signature) (symbol? signature))   #t)
              ((symbol? (car signature))                    (loop (cdr signature)))
              ((and (list? (car signature))
                    (symbol? (car (car signature)))
                    (specializer? (cadr (car signature))) ) (loop (cdr signature)))
              (else #f) ) ) )

    ;; This is not just `nonstrict-subclass?` to support mixins as specializers.
    ;; Mixin classes can be arbitrary classes: just subclasses of <object>, for
    ;; example. Ability to specialize generics on _all_ mixins (not only the
    ;; ones already mixed into some concrete classes) is a good one, so we treat
    ;; such classes as valid method specializers for any generic specializer.
    ;;
    ;; For example, <procedure> is such mixin: it can be added to any class to
    ;; make its instances funcallable. This is used to define <generic> and
    ;; specialization on <procedure> is used to install proper direct accessors.
    ;;
    ;; TODO: At the moment 'mixin' = 'direct and only subclass of <object>'.
    ;;       This should be refined in future when proper abstract classes are
    ;;       implemented. Then 'mixin' can be defined as 'any abstract class'.
    ;;
    (define (coherent-specializer? method-specializer generic-specializer)
      (assert (specializer? method-specializer)
              (specializer? generic-specializer) )
      (or (let ((superclasses (class-all-superclasses-ref method-specializer)))
            (and (= 1 (length superclasses))
                 (eq? <object> (car superclasses)) ) )
          (nonstrict-subclass? method-specializer generic-specializer) ) )

    (define (signatures-coherent? method generic)
      (assert (valid-signature? (method-signature-ref method))
              (valid-signature? (generic-signature-ref generic)) )
      (let loop ((ms (method-signature-ref method))
                 (gs (generic-signature-ref generic)) )
        (cond ((or (both null? ms gs) (both symbol? ms gs))    #t)
              ((both symbol? (car ms) (car gs))                (loop (cdr ms) (cdr gs)))
              ((and (both list? (car ms) (car gs))
                    (coherent-specializer? (cadr (car ms))
                                           (cadr (car gs)) ) ) (loop (cdr ms) (cdr gs)))
              (else #f) ) ) )

) )
