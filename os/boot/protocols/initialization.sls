#!r6rs
(library (os boot protocols initialization)
  ;
  ;   Other metaobject initialization (part of protocols/instantiation)
  ;
  (export initialize)

  (import (except (rnrs base) assert)
          (rnrs control)
          (only (srfi :1) any)
          (os meta accessors)
          (os internal slot-access)
          (os internal signature-checks)
          (os boot meta classes)
          (os boot meta generics)
          (os boot macros predefine-method)
          (os utils misc) )

  (begin

    (predefine-method (initialize call-next-method generic initargs) `((generic ,<generic>) initargs)
      (call-next-method)

      (unless (valid-signature? (signature generic))
        (error #f "invalid signature" generic (signature generic)) )

      (set-effective-function! generic
        (lambda args
          (error #f "no applicable method" (name generic) args) ) )

      (set-significant-positions! generic
        (compute-significant-positions (signature generic)) )

      generic )

    (predefine-method (initialize call-next-method method initargs) `((method ,<method>) initargs)
      (call-next-method)

      (unless (valid-signature? (signature method))
        (error #f "invalid signature" method (signature method)) )

      (set-discriminators! method
        (filter-discriminators (signature method)) )

      method )

    (define (compute-significant-positions signature)
      (indices-of pair? signature ) )

    (define (filter-discriminators signature)
      (proper-filter-map
        (lambda (spec) (if (pair? spec) (cadr spec) #f))
        signature ) )

    (predefine-method (initialize call-next-method eslot initargs) `((eslot ,<effective-slot>) initargs)
      (call-next-method)

      (let ((has-init-value (slot-bound? eslot 'init-value))
            (has-init-thunk (slot-bound? eslot 'init-thunk))
            (init-required  (init-required? eslot))
            (init-keyword   (init-keyword eslot))
            (allocation     (allocation eslot)) )

        (unless (any (lambda (x) (eq? allocation x))
                  '(instance each-subclass class-lineage virtual) )
          (error #f "unsupported slot allocation" (name eslot) allocation) )

        (when (and (eq? allocation 'virtual)
                   (or init-keyword init-required
                       has-init-value has-init-thunk ) )
          (error #f "virtual slots cannot have initialization specifiers" (name eslot) initargs) )

        (when (and has-init-value has-init-thunk)
          (error #f "slot has both init-value and init-thunk defined" (name eslot) initargs) )

        (when has-init-thunk
          (unless (procedure? (init-thunk eslot))
            (error #f "init-thunk is not a procedure" (name eslot) initargs) ) )

        (unless (implies init-required init-keyword)
          (error #f "slot is init-required but has no init-keyword" (name eslot) initargs) )

        (when (and init-required (or has-init-value has-init-thunk))
          (error #f "slot is init-required while having init-value" (name eslot) initargs) ) )

      eslot )

) )
