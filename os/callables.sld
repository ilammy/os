(define-library (os callables)
  ;
  ;   Callable objects
  ;
  (import (scheme base)
          (srfi 69) ) ; hash-tables

  (export callable? object-of add-callable!)

  (begin

    (define *callables* (make-hash-table eq?))

    (define (callable? procedure)
      (hash-table-exists? *callables* procedure) )

    (define (object-of procedure)
      (if (procedure? procedure)
          (hash-table-ref *callables* procedure)
          procedure ) )

    (define (add-callable! procedure object)
      (hash-table-set! *callables* procedure object) )

) )
