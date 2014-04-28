#!r6rs
(library (os internal pretty-printing)

  (export to-write to-display)

  (import (rnrs base)
          (os macros define-generic)
          (os macros define-method)
          (os meta accessors)
          (os meta classes) )

  (begin

    (define-generic to-write   ((object)))
    (define-generic to-display ((object)))

    (define-method (to-write $ object) (<object>)
      "#<object>" )

    (define-method (to-display $ object) (<object>)
      "#<object>" )

) )
