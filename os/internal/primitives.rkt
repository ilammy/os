#lang racket

(require racket/contract)

(provide make-primitive
         make-primitive-<class>
         primitive?
         primitive-class
         primitive-ref
         primitive-set!
         make-uninitialized-vector
         undefined-slot-value?

         (contract-out
           (set-primitive-write-proc!   (-> (-> primitive? string?) (-> primitive? string?)))
           (set-primitive-display-proc! (-> (-> primitive? string?) (-> primitive? string?))) ) )

;
;;  Pretty-printing hooks
;

(define (default-write-proc object) "#<primitive>")
(define write-proc   default-write-proc)
(define display-proc default-write-proc)

(define (set-primitive-write-proc! new-value)
  (let ([previous-value write-proc])
    (set! write-proc new-value)
    previous-value ) )

(define (set-primitive-display-proc! new-value)
  (let ([previous-value display-proc])
    (set! display-proc new-value)
    previous-value ) )

(define (primitive-write-dispatcher object port mode)
  (let* ([serialize (if mode write-proc display-proc)]
         [representation (serialize object)])
    (write-string representation port) ) )

;
;;  Undefined slot value
;

(struct undefined ())

(define *undefined-slot-value* (undefined))

(define (undefined-slot-value? value)
  (eq? *undefined-slot-value* value) )

;
;;  Primititve objects
;

(struct primitive
  ([class #:mutable] slots)
  #:methods gen:custom-write
    [(define write-proc primitive-write-dispatcher)] )

(define (make-primitive class slot-count)
  (primitive class
    (make-uninitialized-vector slot-count) ) )

(define (make-primitive-<class> slot-count)
  (let ([primitive (make-primitive #f slot-count)])
    (set-primitive-class! primitive primitive)
    primitive ) )

(define (make-uninitialized-vector size)
  (make-vector size *undefined-slot-value*) )

(define (primitive-ref primitive nth)
  (vector-ref (primitive-slots primitive) nth) )

(define (primitive-set! primitive nth value)
  (vector-set! (primitive-slots primitive) nth value) )
