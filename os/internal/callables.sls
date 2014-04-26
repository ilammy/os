#!r6rs
(library (os internal callables)
  ;
  ;   Callable objects
  ;
  (export callable? object-of add-callable!)

  (import (rnrs base)
          (srfi :69) ) ; hash-tables

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
