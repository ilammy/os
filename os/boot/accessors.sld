(define-library (os boot accessors)
  ;
  ;   Utility functions for initargs
  ;
  (import (scheme base)
          (os primitives) )

  (export (rename primitive-class object-class-ref)

          class-name-ref
          class-name-set!
          class-direct-superclasses-ref
          class-direct-superclasses-set!
          class-direct-slots-ref
          class-direct-slots-set!
          class-all-superclasses-ref
          class-all-superclasses-set!
          class-all-slots-ref
          class-all-slots-set!
          class-instance-size-ref
          class-instance-size-set!

          slot-name-ref
          slot-name-set!
          slot-init-keyword-ref
          slot-init-keyword-set!
          slot-getter-ref
          slot-getter-set!
          slot-setter-ref
          slot-setter-set!

          effective-slot-direct-getter-ref
          effective-slot-direct-getter-set!
          effective-slot-direct-setter-ref
          effective-slot-direct-setter-set! )

  (begin

    (define-syntax define-primitive-accessor-pair
      (syntax-rules ()
        ((_ i getter setter)
         (begin (define (getter o)   (primitive-ref  o i))
                (define (setter o v) (primitive-set! o i v)) ) ) ) )

    (define-syntax define-primitive-accessors
      (syntax-rules ()
        ((_ (specs ...)) (define-primitive-accessor-pair specs ...))
        ((_ (specs ...) other-specs ...)
         (begin (define-primitive-accessor-pair specs ...)
                (define-primitive-accessors other-specs ...) ) ) ) )

    (define-primitive-accessors
      (0 class-name-ref
         class-name-set! )
      (1 class-direct-superclasses-ref
         class-direct-superclasses-set! )
      (2 class-direct-slots-ref
         class-direct-slots-set! )
      (3 class-all-superclasses-ref
         class-all-superclasses-set! )
      (4 class-all-slots-ref
         class-all-slots-set! )
      (5 class-instance-size-ref
         class-instance-size-set! ) )

    (define-primitive-accessors
      (0 slot-name-ref
         slot-name-set! )
      (1 slot-init-keyword-ref
         slot-init-keyword-set! )
      (2 slot-getter-ref
         slot-getter-set! )
      (3 slot-setter-ref
         slot-setter-set! ) )

    (define-primitive-accessors
      (4 effective-slot-direct-getter-ref
         effective-slot-direct-getter-set! )
      (5 effective-slot-direct-setter-ref
         effective-slot-direct-setter-set! ) )

) )
