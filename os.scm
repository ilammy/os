;;;
;;; R5RS compatibility definitions: error, when, unless, let-values,
;;;                                 ternary assoc, ternary member
;;;
;;; SRFI-1: fold, find, filter, every
;;;
;;; SRFI-2: and-let*
;;;
;;; SRFI-69: make-hash-table, hash-table-exists?, hash-table-ref,
;;;          hash-table-set!, hash-table-update!, hash-table-values
;;;
;;; SRFI-95: sort
;;;

;;;
;;; Primitive objects
;;;

; Interface:
;
;   make-primitive, make-primitive-<class>, primitive?,
;   primitive-class, primitive-ref, primitive-set!,
;   undefined-slot-value?

;;;
;;; Metaobject slot accessors
;;;

;; Public:
(define (name                    o)   (slot-ref o  'name                ))
(define (direct-superclasses     o)   (slot-ref o  'direct-superclasses ))
(define (direct-slots            o)   (slot-ref o  'direct-slots        ))
(define (all-superclasses        o)   (slot-ref o  'all-superclasses    ))
(define (all-slots               o)   (slot-ref o  'all-slots           ))
(define (init-keyword            o)   (slot-ref o  'init-keyword        ))
(define (getter                  o)   (slot-ref o  'getter              ))
(define (setter                  o)   (slot-ref o  'setter              ))
(define (methods                 o)   (slot-ref o  'methods             ))
(define (signature               o)   (slot-ref o  'signature           ))
(define (discriminators          o)   (slot-ref o  'discriminators      ))

;; Private:
(define (instance-size           o)   (slot-ref o  'instance-size       ))
(define (direct-getter           o)   (slot-ref o  'direct-getter       ))
(define (direct-setter           o)   (slot-ref o  'direct-setter       ))
(define (effective-function      o)   (slot-ref o  'effective-function  ))
(define (method-body             o)   (slot-ref o  'method-body         ))

(define (set-all-superclasses!   o v) (slot-set! o 'all-superclasses   v))
(define (set-all-slots!          o v) (slot-set! o 'all-slots          v))
(define (set-instance-size!      o v) (slot-set! o 'instance-size      v))
(define (set-direct-getter!      o v) (slot-set! o 'direct-getter      v))
(define (set-direct-setter!      o v) (slot-set! o 'direct-setter      v))
(define (set-methods!            o v) (slot-set! o 'methods            v))
(define (set-effective-function! o v) (slot-set! o 'effective-function v))

;;;
;;; Class descriptions
;;;

(define <object>
  (make <class>
    'name: '<object>
    'direct-superclasses: '()
    'direct-slots: '() ) )

(define <class>
  (make <class>
    'name: '<class>
    'direct-superclasses: (list <object>)
    'direct-slots:
      (list
        (make <slot>
          'name:         'name
          'init-keyword: 'name:
          'getter:        name )

        (make <slot>
          'name:         'direct-superclasses
          'init-keyword: 'direct-superclasses:
          'getter:        direct-superclasses )

        (make <slot>
          'name:         'direct-slots
          'init-keyword: 'direct-slots:
          'getter:        direct-slots )

        (make <slot>
          'name:  'all-superclasses
          'getter: all-superclasses )

        (make <slot>
          'name:  'all-slots
          'getter: all-slots )

        (make <slot> 'name: 'instance-size) ) ) )

(define <slot>
  (make <class>
    'name: '<slot>
    'direct-superclasses: (list <object>)
    'direct-slots:
      (list
        (make <slot>
          'name:         'name
          'init-keyword: 'name:
          'getter:        name )

        (make <slot>
          'name:         'init-keyword
          'init-keyword: 'init-keyword:
          'getter:        init-keyword )

        (make <slot>
          'name:         'getter
          'init-keyword: 'getter:
          'getter:        getter )

        (make <slot>
          'name:         'setter
          'init-keyword: 'setter:
          'getter:        setter ) ) ) )

(define <effective-slot>
  (make <class>
    'name: '<effective-slot>
    'direct-superclasses: (list <slot>)
    'direct-slots:
      (list (make <slot> 'name: 'direct-getter)
            (make <slot> 'name: 'direct-setter) ) ) )

(define <generic>
  (make <class>
    'name: '<generic>
    'direct-superclasses: (list <procedure>)
    'direct-slots:
      (list
        (make <slot>
          'name:         'name
          'init-keyword: 'name:
          'getter:        name )

        (make <slot>
          'name:         'signature
          'init-keyword: 'signature:
          'getter:        signature )

        (make <slot>
          'name:  'methods
          'getter: methods )

        (make <slot> 'name: 'effective-function) ) ) )

(define <method>
  (make <class>
    'name: '<method>
    'direct-superclasses: (list <object>)
    'direct-slots:
      (list
        (make <slot>
          'name:         'discriminators
          'init-keyword: 'discriminators:
          'getter:        discriminators )

        (make <slot>
          'name:         'method-body
          'init-keyword: 'method-body: ) ) ) )

(define <procedure>
  (make <class>
    'name: '<procedure>
    'direct-superclasses: (list <object>)
    'direct-slots: '() ) )

;;;
;;; Classifier
;;;

(define (class-of object)
  (cond ((primitive? object) (object-class-ref object))
        ((procedure? object) <procedure>)
        (else (error "unsupported object type" object)) ) )

;;;
;;; Various utils
;;;

(define (for-each-with-index proc list)
  (let loop ((index 0)
             (list list) )
    (unless (null? list)
      (proc index (car list))
      (loop (+ 1 index) (cdr list)) ) ) )

(define (graph-bfs root-node adjacent-nodes node-equal?)
  (let loop ((visited-nodes (list root-node))
             (current-queue (adjacent-nodes root-node))
             (pending-queue '()) )
    (if (null? current-queue)
        (if (null? pending-queue)
            (reverse visited-nodes)
            (loop visited-nodes
                  (reverse pending-queue)
                  '() ) )
        (if (member (car current-queue) visited-nodes node-equal?)
            (loop visited-nodes
                  (cdr current-queue)
                  pending-queue )
            (loop (cons (car current-queue) visited-nodes)
                  (cdr current-queue)
                  (reverse-prepend (adjacent-nodes (car current-queue))
                                   pending-queue ) ) ) ) ) )

(define (reverse-prepend list1 list2)
  (fold cons list2 list1) )

;;;
;;; Initialization
;;;

(define (make class . initargs)
  (let ((object (make-primitive class (instance-size class))))
    (initialize-object! object initargs)
    object ) )

(define (initialize-object! object initargs)
  (for-each (lambda (slot)
              (init-slot-with-initargs! slot object initargs) )
    (all-slots (class-of object)) ) )

(define (init-slot-with-initargs! slot object initargs)
  (and-let* ((keyword (init-keyword slot)))
    (let-values (((keyword-found? value) (get-initarg keyword initargs)))
      (when keyword-found?
        (let ((slot-set! (direct-setter slot)))
          (slot-set! object value) ) ) ) ) )

(define (get-initarg keyword initargs)
  (cond ((null? initargs)              (values #f #f))
        ((eqv? keyword (car initargs)) (values #t (cadr initargs)))
        (else (get-initarg keyword (cddr initargs))) ) )

;;;
;;; Slot access
;;;

(define (slot-ref object slot-name)
  (let* ((class (class-of object))
         (slot (find-slot-by-name class slot-name))
         (slot-ref (direct-getter slot)) )
    (let ((value (slot-ref object)))
      (if (undefined-slot-value? value)
          (error "uninitialized slot" (name class) slot-name)
          value ) ) ) )

(define (slot-set! object slot-name value)
  (let* ((class (class-of object))
         (slot (find-slot-by-name class slot-name))
         (slot-set! (direct-setter slot)) )
    (slot-set! object value) ) )

(define (slot-bound? object slot-name)
  (let* ((class (class-of object))
         (slot (find-slot-by-name class slot-name))
         (slot-ref (direct-getter slot)) )
    (let ((value (slot-ref object)))
      (if (undefined-slot-value? value) #f #t) ) ) )

(define (find-slot-by-name class slot-name)
  (let scan ((slots (all-slots class)))
    (cond ((null? slots) (error "unknown slot" (name class) slot-name))
          ((eq? slot-name (name (car slots))) (car slots))
          (else (scan (cdr slots))) ) ) )

;;;
;;; Class inheritance
;;;

(define (make-class . initargs)
  (let ((class (apply make <class> initargs)))
    (set-all-superclasses! class
      (cdr (graph-bfs class direct-superclasses eq?)) ) ; exclude the class itself

    (let ((all-slots
           (map (lambda (slots)
                  (make <effective-slot>
                    'name:         (name (car slots))
                    'init-keyword: (first-bound 'init-keyword slots)
                    'getter:       (first-bound 'getter       slots)
                    'setter:       (first-bound 'setter       slots) ) )
             (group-slots-by-name class) ) ))

      (for-each-with-index
        (lambda (index slot)
          (set-direct-getter! slot (lambda (o)   (primitive-ref  o index)))
          (set-direct-setter! slot (lambda (o v) (primitive-set! o index v))) )
        all-slots )

      (set-all-slots!     class all-slots)
      (set-instance-size! class (length all-slots)) )

    class ) )

(define (first-bound slot-name objects)
  (let ((slot-bound? (lambda (object) (slot-bound? object slot-name))))
    (and-let* ((object (find slot-bound? objects)))
      (slot-ref object slot-name) ) ) )

(define (group-slots-by-name class)
  (let ((hash (make-hash-table eq?)))
    (for-each
      (lambda (direct-slot)
        (let ((slot-name (name direct-slot)))
          (if (hash-table-exists? hash slot-name)
              (error "duplicate direct slot" (name class) slot-name)
              (hash-table-set! hash slot-name (list direct-slot)) ) ) )
      (direct-slots class) )

    (for-each
      (lambda (superclass)
        (for-each
          (lambda (inherited-slot)
            (hash-table-update! hash (name inherited-slot)
              (lambda (slots) (cons inherited-slot slots))
              (lambda () (list inherited-slot)) ) )
          (direct-slots superclass) ) )
      (all-superclasses class) )

    (map reverse (hash-table-values hash)) ) )

;;;
;;; Generic initialization
;;;

(define (make-generic . initargs)
  (let ((generic (apply make <generic> initargs)))
    (set-methods! generic '())
    (set-effective-function! generic
      (lambda args (error "no applicable method" (name generic) args)) )

    generic ) )

;;;
;;; Generic maintenance
;;;

(define (add-method! generic method)
  (let ((methods (cons method (methods generic))))
    (set-methods! generic methods)

    (set-effective-function! generic
      (lambda args
        (let* ((discriminators (map class-of (discriminator-args generic args)))
               (applicable-methods (applicable-methods generic discriminators)) )
          (if (null? applicable-methods)
              (error "no applicable methods" (name generic) args)
              (apply (method-body (car applicable-methods)) args) ) ) ) ) ) )

(define (discriminator-args generic args)
  (let loop ((result '())
             (signature (signature generic))
             (args args) )
    (cond ((null? signature) (reverse result))
          ((pair? (car signature))
           (loop (cons (car args) result)
                 (cdr signature)
                 (cdr args) ) )
          (else (loop result
                      (cdr signature)
                      (cdr args) )) ) ) )

(define (applicable-methods generic classes)
  (let ((all-methods    (methods generic))
        (applicable?    (lambda (method) (method-applicable? method classes)))
        (more-specific? (lambda (lhs rhs) (more-specific-method? lhs rhs classes))) )
    (sort (filter applicable? all-methods) more-specific?) ) )

(define (method-applicable? method classes)
  (every nonstrict-subclass? classes (discriminators method)) )

; lhs < rhs
(define (more-specific-method? left-method right-method argument-classes)
  (let loop ((L (discriminators left-method))
             (R (discriminators right-method))
             (A argument-classes) )
    (cond ((null? L) #f)
          ((eq? (car L) (car R))
           (loop (cdr L)
                 (cdr R)
                 (cdr A) ) )
          ((subclass? (car L)
                      (car R) ) #t)
          ((memq (car R)
                 (memq (car L)
                       (all-superclasses (car A)) ) ) #t)
          (else (loop (cdr L)
                      (cdr R)
                      (cdr A) )) ) ) )

(define (subclass? class superclass)
  (if (memq superclass (all-superclasses class)) #t #f) )

(define (nonstrict-subclass? class superclass)
  (or (eq? class superclass)
      (subclass? class superclass) ) )
