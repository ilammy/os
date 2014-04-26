#!r6rs
(library (os)
  ;
  ;   User layer of OS
  ;
  (export ; define-class
          define-generic
          define-method

          make <object> )

  (import (os boot complete)
          (os meta classes)
          (os macros define-generic)
          (os macros define-method)
          (os protocols instantiation) ) )
