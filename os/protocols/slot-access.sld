(define-library (os protocols slot-access)
  ;
  ;   Slot access metaobject protocol
  ;
  (import (os boot meta generics))

  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class? ) )
