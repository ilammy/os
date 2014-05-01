#lang racket

(provide callable?
         object-of
         add-callable )

(define *callables* (make-weak-hasheq))

(define (callable-write-proc callable port mode)
  ((if mode write display) (object-of callable) port) )

(struct callable-struct
  (proc)
  #:property prop:procedure
    (struct-field-index proc)
  #:methods gen:custom-write
    [(define write-proc callable-write-proc)] )

(define (callable? object)
  (and (callable-struct? object)
       (hash-has-key? *callables* object) ) )

(define (object-of object)
  (if (callable? object)
      (hash-ref *callables* object)
      object ) )

(define (add-callable procedure object)
  (let ([callable (callable-struct procedure)])
    (hash-set! *callables* callable object)
    callable ) )
