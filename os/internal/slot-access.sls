#!r6rs
(library (os internal slot-access)
  ;
  ;   Slot access
  ;
  (export slot-ref slot-set! slot-bound?)

  (import (except (rnrs base) assert)
          (os internal class-of)
          (os protocols slot-access)
          (os meta classes)
          (os boot internal slot-access) )

  (begin

    (define (slot-ref object slot-name)
      (let ((class (class-of object)))
        (if (eq? <class> (class-of class))
            (slot-ref-in-<class> class object slot-name)
            (slot-ref-in-class   class object slot-name) ) ) )

    (define (slot-set! object slot-name value)
      (let ((class (class-of object)))
        (if (eq? <class> (class-of class))
            (slot-set-in-<class>! class object slot-name value)
            (slot-set-in-class!   class object slot-name value) ) ) )

    (define (slot-bound? object slot-name)
      (let ((class (class-of object)))
        (if (eq? <class> (class-of class))
            (slot-bound-in-<class>? class object slot-name)
            (slot-bound-in-class?   class object slot-name) ) ) )

) )
