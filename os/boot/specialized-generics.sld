(define-library (os boot specialized-generics)

  (import (scheme base)
          (scheme write)
          (os accessors)
          (os boot classes definitions)
          (os boot generics definitions)
          (os boot methods predefine) )

  (export test-generic)

  (begin

    (predefine-method (test-generic class) (<class>)
      (display "<class>") (newline)
      (display "name: ") (display (name class)) (newline) )

) )
