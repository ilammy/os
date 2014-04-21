(define-library (os meta accessors)
  ;
  ;   Standard metaobject accessors
  ;
  (import (scheme base)
          (os internal slot-access) )

  (export name
          direct-superclasses
          direct-slots
          init-keyword
          getter
          setter
          signature
          method-combinator
          discriminators
          method-body
          all-superclasses   set-all-superclasses!
          all-slots          set-all-slots!
          instance-size      set-instance-size!
          direct-getter      set-direct-getter!
          direct-setter      set-direct-setter!
          methods            set-methods!
          effective-function set-effective-function! )

  (begin

    (define-syntax def-ref-set
      (syntax-rules ()
        ((_ slot getter)
         (define (getter o) (slot-ref o 'slot)) )
        ((_ slot getter setter)
         (begin (define (getter o)   (slot-ref o 'slot))
                (define (setter o v) (slot-set! o 'slot v)) ) ) ) )

    (def-ref-set name                name)
    (def-ref-set direct-superclasses direct-superclasses)
    (def-ref-set direct-slots        direct-slots)
    (def-ref-set init-keyword        init-keyword)
    (def-ref-set getter              getter)
    (def-ref-set setter              setter)
    (def-ref-set signature           signature)
    (def-ref-set method-combinator   method-combinator)
    (def-ref-set discriminators      discriminators)
    (def-ref-set method-body         method-body)

    (def-ref-set all-superclasses    all-superclasses   set-all-superclasses!)
    (def-ref-set all-slots           all-slots          set-all-slots!)
    (def-ref-set instance-size       instance-size      set-instance-size!)
    (def-ref-set direct-getter       direct-getter      set-direct-getter!)
    (def-ref-set direct-setter       direct-setter      set-direct-setter!)
    (def-ref-set methods             methods            set-methods!)
    (def-ref-set effective-function  effective-function set-effective-function!)

) )
