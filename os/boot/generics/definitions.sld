(define-library (os boot generics definitions)
  ;
  ;   Metaobject definitions
  ;
  (import (scheme base)
          (os boot generics predefine) )

  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class? )

  (begin

    ; protocols/slot-access
    ;
    (predefine-generic slot-ref-in-class    ((object) (class) slot-name))
    (predefine-generic slot-set-in-class!   ((object) (class) slot-name value))
    (predefine-generic slot-bound-in-class? ((object) (class) slot-name))

) )
