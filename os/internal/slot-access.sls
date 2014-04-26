#!r6rs
(library (os internal slot-access)
  ;
  ;   Slot access
  ;
  (export slot-ref slot-set! slot-bound?)

  (import (rnrs base)
          (os internal class-of)
          (os protocols slot-access) )

  (begin

    (define (slot-ref object slot-name)
      (slot-ref-in-class object (class-of object) slot-name) )

    (define (slot-set! object slot-name value)
      (slot-set-in-class! object (class-of object) slot-name value) )

    (define (slot-bound? object slot-name)
      (slot-bound-in-class? object (class-of object) slot-name) )

) )
