(define-library (os boot protocols initialization)
  ;
  ;   Other metaobject initialization (part of protocols/instantiation)
  ;
  (import (scheme base)
          (os meta accessors)
          (os internal slot-access)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils misc) )

  (export initialize)

  (begin

    (predefine-method (initialize call-next-method generic initargs) (<generic>)
      (call-next-method)

      (set-effective-function! generic
        (lambda args
          (error "no applicable method" (name generic) args) ) )

      generic )

    (predefine-method (initialize call-next-method eslot initargs) (<effective-slot>)
      (call-next-method)

      (let ((has-init-value (slot-bound? eslot 'init-value))
            (has-init-thunk (slot-bound? eslot 'init-thunk))
            (init-required  (init-required? eslot))
            (init-keyword   (init-keyword eslot)) )

        (when (and has-init-value has-init-thunk)
          (error "slot has both init-value and init-thunk defined" (name eslot) initargs) )

        (when has-init-thunk
          (unless (procedure? (init-thunk eslot))
            (error "init-thunk is not a procedure" (name eslot) initargs) ) )

        (unless (implies init-required init-keyword)
          (error "slot is init-required but has no init-keyword" (name eslot) initargs) )

        (when (and init-required (or has-init-value has-init-thunk))
          (error "slot is init-required while having init-value" (name eslot) initargs) ) )

      eslot )

) )
