(define-library (os boot protocols initialization)
  ;
  ;   Other metaobject initialization (part of protocols/instantiation)
  ;
  (import (scheme base)
          (os meta accessors)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method) )

  (export initialize)

  (begin

    (predefine-method (initialize call-next-method generic initargs) (<generic>)
      (call-next-method)

      (set-effective-function! generic
        (lambda args
          (error "no applicable method" (name generic) args) ) )

      generic )

) )
