#!r6rs
(library (os utils initargs)
  ;
  ;   Utility functions for initargs
  ;
  (export for-each-initarg
          get-initarg )

  (import (except (rnrs base) assert)
          (rnrs control)
          (only (srfi :1) first second drop) )

  (begin

    (define (for-each-initarg proc initargs)
      (unless (null? initargs)
        (proc (first initargs) (second initargs))
        (for-each-initarg proc (drop initargs 2)) ) )

    (define (get-initarg keyword initargs)
      (cond ((null? initargs)               (values #f #f))
            ((eq? keyword (first initargs)) (values #t (second initargs)))
            (else (get-initarg keyword (drop initargs 2))) ) )

) )
