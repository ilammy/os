(define-library (os boot generics definitions)
  ;
  ;   Metaobject definitions
  ;
  (import (scheme base)
          (os boot generics predefine) )

  (export test-generic)

  (begin

    (predefine-generic test-generic ((class)))

) )
