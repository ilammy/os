#!r6rs
(library (os internal virtual-slots)
  ;
  ;   Implementation of protocols/slot-access
  ;
  (export virtual-getter
          virtual-setter )

  (import (except (rnrs base) assert)
          (os meta accessors)
          (os utils slots) )

  (begin

    (define (virtual-getter class slot-name)
      (let ((slot (find-slot-by-name class slot-name)))
        (if (eq? 'virtual (allocation slot))
            (direct-getter slot)
            (error #f "not a virtual slot" class slot-name) ) ) )

    (define (virtual-setter class slot-name)
      (let ((slot (find-slot-by-name class slot-name)))
        (if (eq? 'virtual (allocation slot))
            (direct-setter slot)
            (error #f "not a virtual slot" class slot-name) ) ) )

) )
