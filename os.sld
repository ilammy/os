(define-library (os)
  ;
  ;   User layer of OS
  ;
  (import (os boot complete)
          (os meta classes)
          (os macros define-generic)
          (os macros define-method)
          (os protocols instantiation) )

  (export ; define-class
          define-generic
          define-method

          make <object> ) )
