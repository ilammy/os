#!r6rs
(library (os protocols slot-access)
  ;
  ;   Slot access metaobject protocol
  ;
  (export slot-ref-in-class
          slot-set-in-class!
          slot-bound-in-class? )

  (import (os boot meta generics)) )
