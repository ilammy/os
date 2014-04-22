(define-library (os boot meta accessors)
  ;
  ;   Utility functions for initargs
  ;
  (import (scheme base)
          (os boot meta classes)
          (os internal primitives)
          (os utils assert) )

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
          slot-init-value-ref
          slot-init-value-set!
          slot-init-thunk-ref
          slot-init-thunk-set!
          slot-getter-ref
          slot-getter-set!
          slot-setter-ref
          slot-setter-set!

          effective-slot-direct-getter-ref
          effective-slot-direct-getter-set!
          effective-slot-direct-setter-ref
          effective-slot-direct-setter-set!

          generic-name-ref
          generic-name-set!
          generic-signature-ref
          generic-signature-set!
          generic-method-combinator-ref
          generic-method-combinator-set!
          generic-methods-ref
          generic-methods-set!
          generic-effective-function-ref
          generic-effective-function-set!

          method-discriminators-ref
          method-discriminators-set!
          method-body-ref
          method-body-set! )

  (begin

    (define-syntax check-class
      (syntax-rules ()
        ((_ accessor object (class ...))
         (assert (primitive? object)
                 (eq? <class> (primitive-class (primitive-class object)))
                 (or (eq? (primitive-class object) class) ...)
                 msg: (class-name-ref (primitive-class object))
                      "is not a valid class for" accessor ) ) ) )

    (define-syntax checked-ref
      (syntax-rules ()
        ((_ expected-classes getter object index)
         (begin (check-class getter object expected-classes)
                (primitive-ref object index) ) ) ) )

    (define-syntax checked-set!
      (syntax-rules ()
        ((_ expected-classes setter object index value)
         (begin (check-class setter object expected-classes)
                (primitive-set! object index value) ) ) ) )

    (define-syntax define-primitive-accessor-pair
      (syntax-rules ()
        ((_ classes i getter setter)
         (begin (define (getter o)   (checked-ref  classes getter o i))
                (define (setter o v) (checked-set! classes setter o i v)) ) ) ) )

    (define-syntax define-primitive-accessors
      (syntax-rules ()
        ((_ classes (specs ...))
         (define-primitive-accessor-pair classes specs ...) )
        ((_ classes (specs ...) other-specs ...)
         (begin (define-primitive-accessor-pair classes specs ...)
                (define-primitive-accessors     classes other-specs ...) ) ) ) )

    (define-primitive-accessors (<class>)
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

    (define-primitive-accessors (<slot> <effective-slot>)
      (0 slot-name-ref
         slot-name-set! )
      (1 slot-init-keyword-ref
         slot-init-keyword-set! )
      (2 slot-init-value-ref
         slot-init-value-set! )
      (3 slot-init-thunk-ref
         slot-init-thunk-set! )
      (4 slot-getter-ref
         slot-getter-set! )
      (5 slot-setter-ref
         slot-setter-set! ) )

    (define-primitive-accessors (<effective-slot>)
      (6 effective-slot-direct-getter-ref
         effective-slot-direct-getter-set! )
      (7 effective-slot-direct-setter-ref
         effective-slot-direct-setter-set! ) )

    (define-primitive-accessors (<generic>)
      (0 generic-name-ref
         generic-name-set! )
      (1 generic-signature-ref
         generic-signature-set! )
      (2 generic-method-combinator-ref
         generic-method-combinator-set! )
      (3 generic-methods-ref
         generic-methods-set! )
      (4 generic-effective-function-ref
         generic-effective-function-set! ) )

    (define-primitive-accessors (<method>)
      (0 method-discriminators-ref
         method-discriminators-set! )
      (1 method-body-ref
         method-body-set! ) )

) )
