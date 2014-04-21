(define-library (os protocols instantiation)
  ;
  ;   Object allocation and instantiation protocol
  ;
  (import (os boot generics definitions))

  (export make
            allocate
            initialize ) )
