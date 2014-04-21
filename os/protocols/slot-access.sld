(define-library (os protocols slot-access)
  ;
  ;   Slot access metaobject protocol
  ;
  (import (os boot generics definitions))

  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class? ) )
