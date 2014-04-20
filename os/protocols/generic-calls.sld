(define-library (os protocols generic-calls)
  ;
  ;   Generic function maintenance metaobject protocol
  ;
  (import (os boot generics definitions))

  (export add-method!
            compute-effective-function
              find-applicable-methods
                more-specific-method?
              compute-effective-method
                compute-method-function ) )
