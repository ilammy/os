(define-library (os utils misc)
  ;
  ;   Miscellaneous utility functions
  ;
  (import (scheme base)
          (only (srfi 1) fold) )

  (export for-each-with-index graph-bfs proper-length)

  (begin

    (define (for-each-with-index proc list)
      (let loop ((index 0)
                 (list list) )
        (unless (null? list)
          (proc index (car list))
          (loop (+ 1 index) (cdr list)) ) ) )

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
        (cond ((null? list)       length)
              ((not (pair? list)) length)
              (else (loop (cdr list)
                          (+ 1 length) )) ) ) )

) )
