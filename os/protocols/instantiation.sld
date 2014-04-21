(define-library (os protocols instantiation)
  ;
  ;   Object allocation and instantiation protocol
  ;
  (import (os boot meta generics))

  (export make
            allocate
            initialize ) )
