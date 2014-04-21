(define-library (os internal slot-access)
  ;
  ;   Slot access
  ;
  (import (scheme base)
          (os internal class-of)
          (os protocols slot-access) )

  (export slot-ref slot-set! slot-bound?)

  (begin

    (define (slot-ref object slot-name)
      (slot-ref-in-class object (class-of object) slot-name) )

    (define (slot-set! object slot-name value)
      (slot-set-in-class! object (class-of object) slot-name value) )

    (define (slot-bound? object slot-name)
      (slot-bound-in-class? object (class-of object) slot-name) )

) )
