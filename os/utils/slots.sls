#!r6rs
(library (os utils slots)
  ;
  ;   Implementation of protocols/slot-access
  ;
  (export find-slot-by-name)

  (import (except (rnrs base) assert)
          (os meta accessors) )

  (begin

    (define (find-slot-by-name class slot-name)
      (let scan ((slots (all-slots class)))
        (cond ((null? slots) (error #f "unknown slot" (name class) slot-name))
              ((eq? slot-name (name (car slots))) (car slots))
              (else (scan (cdr slots))) ) ) )

) )
