#!r6rs
(library (os internal pretty-printing)

  (export to-write to-display)

  (import (rnrs base)
          (only (srfi :1) any)
          (only (srfi :13) string-trim string-trim-right)
          (os macros define-generic)
          (os macros define-method)
          (os meta accessors)
          (os meta classes)
          (os internal class-of) )

  (begin

    (define-generic to-write   ((object)))
    (define-generic to-display ((object)))

    (define-method (to-write $ object) `((object ,<object>))
      (object->string object) )

    (define-method (to-display $ object) `((object ,<object>))
      (object->string object) )

    (define (object->string object)
      (string-append
        "#<"
        (clean-name (class-of object))
        (if (has-slot? 'name object)
            (string-append ":" (clean-name object))
            "" )
        ">" ) )

    (define (clean-name object)
      (trim #\< (symbol->string (name object)) #\>) )

    (define (trim left string right)
      (string-trim-right (string-trim string left) right) )

    (define (has-slot? slot-name object)
      (any (lambda (slot) (eq? slot-name (name slot)))
        (all-slots (class-of object)) ) )

    'dummy
) )
