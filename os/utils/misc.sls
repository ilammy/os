#!r6rs
(library (os utils misc)
  ;
  ;   Miscellaneous utility functions
  ;
  (export for-each-with-index
          indices-of
          graph-bfs
          proper-length
          proper-filter-map
          implies
          both
          xor )

  (import (except (rnrs base) assert)
          (rnrs control)
          (only (srfi :1) fold member) )

  (begin

    (define (for-each-with-index proc list)
      (let loop ((index 0)
                 (list list) )
        (unless (null? list)
          (proc index (car list))
          (loop (+ 1 index) (cdr list)) ) ) )

    (define (indices-of pred? list)
      (let loop ((result '())
                 (list list)
                 (index 0) )
        (cond ((not (pair? list)) (reverse result))
              ((pred? (car list))
               (loop (cons index result) (cdr list) (+ 1 index)))
              (else (loop result (cdr list) (+ 1 index))) ) ) )

    (define (graph-bfs root-node adjacent-nodes node-equal?)
      (let loop ((visited-nodes (list root-node))
                 (current-queue (adjacent-nodes root-node))
                 (pending-queue '()) )
        (if (null? current-queue)
            (if (null? pending-queue)
                (reverse visited-nodes)
                (loop visited-nodes
                      (reverse pending-queue)
                      '() ) )
            (if (member (car current-queue) visited-nodes node-equal?)
                (loop visited-nodes
                      (cdr current-queue)
                      pending-queue )
                (loop (cons (car current-queue) visited-nodes)
                      (cdr current-queue)
                      (reverse-prepend (adjacent-nodes (car current-queue))
                                       pending-queue ) ) ) ) ) )

    (define (reverse-prepend list1 list2)
      (fold cons list2 list1) )

    (define (proper-length list)
      (let loop ((list   list)
                 (length 0) )
        (cond ((not (pair? list)) length)
              (else (loop (cdr list)
                          (+ 1 length) )) ) ) )

    (define (proper-filter-map pred? list)
      (let loop ((result '())
                 (list list))
        (cond ((not (pair? list)) (reverse result))
              ((pred? (car list)) (loop (cons (pred? (car list)) result)
                                        (cdr list) ))
              (else (loop result (cdr list))) ) ) )

    (define (implies premise consequence) (or (not premise) consequence))

    (define (both p a b) (and (p a) (p b)))

    (define (xor p q) (and (or p q) (not (and p q))))

) )
