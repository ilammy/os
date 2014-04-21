(define-library (os assert)
  ; ;
   ;  Provides an assertion macro for checking invariants.
  ; ;
   ;  SYNTAX
  ; ;
   ;      (assert <test-expression>+
  ; ;             [msg: <message-expression>+]? )
   ;
  ; ; SEMANTICS
   ;
  ; ;     If evaluation of (and <test-expressions>) yields a true value then
   ;      nothing happens. Otherwise an error is signalled. If the "msg:" is
  ; ;     specified then <message-expressions> are evaluated and printed out,
   ;      otherwise the failing <test-expression> will be the only message.
  ; ;
   ;      The value of an assertion expression is unspecified.
  ; ;
   ;  CONFIGURATION
  ; ;
   ;      Parameters:
  ; ;
   ;        - (current-error-port)
  ; ;
   ;          Failure messages are printed into this port.
  ; ;
   ;      Features:
  ; ;
   ;        - assertions-nonfatal
  ; ;
   ;          Test expressions are evaluated, messages are printed out in case
  ; ;         of failure, but no errors are signalled.
   ;
  ; ;       - assertions-disabled
   ;
  ; ;         Nothing is ever evaluated. (assert ...) is equivalent to (begin).
   ;          Syntax is still checked for correctness though.
  ; ;
  (import (scheme base) (scheme write))

  (export assert)

  (begin

    (define-syntax display-message
      (syntax-rules ()
        ((_ msg) (display msg (current-error-port)))
        ((_ msg1 msg2 ...)
         (begin (display msg1 (current-error-port))
                (display " " (current-error-port))
                (display-message msg2 ...) ) ) ) )

    (define-syntax print-failure-message
      (syntax-rules ()
        ((_ failing-test #f)
         (begin
           (display "*** ASSERTION FAILED ***" (current-error-port))
           (newline (current-error-port))
           (display "Failed test: " (current-error-port))
           (display 'failing-test (current-error-port))
           (newline (current-error-port)) ) )
        ((_ failing-test (messages ...))
         (begin
           (display "*** ASSERTION FAILED ***" (current-error-port))
           (newline (current-error-port))
           (display "Failed test: " (current-error-port))
           (display 'failing-test (current-error-port))
           (newline (current-error-port))
           (display-message messages ...)
           (newline (current-error-port)) ) ) ) )

    (define-syntax raise-assertion-error
      (syntax-rules ()
        ((_)
         (cond-expand
           (assertions-nonfatal (begin))
           (else (error "failed assertion")) ) ) ) )

    (define-syntax report-failure
      (syntax-rules ()
        ((_ failing-test messages)
         (cond-expand
           (assertions-disabled (begin))
           (else (begin (print-failure-message failing-test messages)
                        (raise-assertion-error) )) ) ) ) )

    (define-syntax do-assertion
      (syntax-rules ()
        ((_ () messages) (begin))
        ((_ (test1 test2 ...) messages)
         (if test1 (do-assertion (test2 ...) messages)
                   (report-failure test1 messages) ) ) ) )

    ; (parse-assertion tests messages custom-message? remaining-args)
    ;
    (define-syntax parse-assertion
      (syntax-rules (msg:)
        ((_ () messages #f ())
         (syntax-error "assertion must have at least one test") )

        ((_ tests messages #f ()) (do-assertion tests #f))
        ((_ tests messages #t ()) (do-assertion tests messages))

        ((_ tests messages #f (msg:))
         (syntax-error "assertion message cannot be empty") )

        ((_ () messages #f (msg: rest ...))
         (syntax-error "assertion must have at least one test") )

        ((_ tests (messages ...) #f (msg: msg rest ...))
         (parse-assertion tests (messages ... msg) #t (rest ...)) )

        ((_ tests messages #t (msg: rest ...))
         (syntax-error "assertion cannot have two messages") )

        ((_ (tests ...) messages #f (test rest ...))
         (parse-assertion (tests ... test) messages #f (rest ...)) )

        ((_ tests (messages ...) #t (msg rest ...))
         (parse-assertion tests (messages ... msg) #t (rest ...)) ) ) )

    (define-syntax assert
      (syntax-rules ()
        ((_ args ...) (parse-assertion () () #f (args ...))) ) )

) )
