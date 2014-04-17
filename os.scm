;;;
;;; R5RS compatibility definitions
;;;

;;
;; Built-ins
;;

(define error 'wait)
(call-with-current-continuation
  (lambda (k)
    (set! error (lambda (msg . args)
                  (display "Error! ")
                  (display msg) (display ": ") (display args)
                  (newline)
                  (k 'abort) ) ) ) )

(define-syntax unless
  (syntax-rules ()
    ((unless condition body1 body2 ...)
     (if condition '()
         (begin body1 body2 ...) ) ) ) )

(define-syntax when
  (syntax-rules ()
    ((when condition body1 body2 ...)
     (if condition
         (begin body1 body2 ...)
         '() ) ) ) )

(define (member x list =)
  (cond ((null? list) #f)
        ((= x (car list)) list)
        (else (member x (cdr list) =)) ) )

(define (assoc k alist =)
  (cond ((null? alist) #f)
        ((= k (car (car alist))) (car alist))
        (else (assoc k (cdr alist) =)) ) )

(define-syntax let-values
  (syntax-rules ()
    ((let-values () body1 body2 ...)
     (begin body1 body2 ...) )
    ((let-values (((var1 var2 ...) exp) other ...) body1 body2 ...)
     (call-with-values
       (lambda () exp)
       (lambda (var1 var2 ...)
         (let-values (other ...) body1 body2 ...) ) ) ) ) )

;;
;; SRFI-1 (list library)
;;

(define first  car)
(define second cadr)

(define (fold proc init list)
  (if (null? list) init
      (fold proc
            (proc (car list) init)
            (cdr list) ) ) )

(define (find pred list)
  (cond ((null? list) #f)
        ((pred (car list)) (car list))
        (else (find pred (cdr list))) ) )

(define (filter pred? list)
  (let loop ((list list)
             (result '()) )
    (if (null? list) (reverse result)
        (loop (cdr list)
              (if (pred? (car list))
                  (cons (car list) result)
                  result ) ) ) ) )

; specialization for 2
(define (every pred? list1 list2)
  (cond ((or (null? list1) (null? list2)) #t)
        ((pred? (car list1) (car list2))
         (every pred? (cdr list1) (cdr list2)) )
        (else #f) ) )

;;
;; SRFI-2 (and-let*)
;;

(define-syntax and-let*
  (syntax-rules ()
    ((and-let* () body1 body2 ...)
     (begin body1 body2 ...) )
    ((and-let* ((var exp) other ...) body1 body2 ...)
     (let ((var exp))
       (and var (and-let* (other ...) body1 body2 ...) ) ) )
    ((and-let* ((exp) other ...) body1 body2 ...)
     (let ((tmp expr))
       (and tmp (and-let* (other ...) body1 body2 ...) ) ) ) ) )

;;
;; SRFI-69 (basic hash tables)
;;

(define (make-hash-table predicate) (cons '() predicate))

(define (hash-table-exists? hash key)
  (let ((data (car hash))
        (eqv? (cdr hash)) )
    (if (assoc key data eqv?) #t #f) ) )

(define (hash-table-ref hash key)
  (let ((data (car hash))
        (eqv? (cdr hash)) )
    (let ((pair (assoc key data eqv?)))
      (if pair (cdr pair)
          (error "reference to unknown key" key) ) ) ) )

(define (hash-table-set! hash key value)
  (let ((data (car hash))
        (eqv? (cdr hash)) )
    (let ((pair (assoc key data eqv?)))
      (if pair
          (set-cdr! pair value)
          (set-car! hash (cons (cons key value) data)) ) ) ) )

(define (hash-table-update! hash key update-thunk init-thunk)
  (let ((data (car hash))
        (eqv? (cdr hash)) )
    (let ((pair (assoc key data eqv?)))
      (if pair
          (set-cdr! pair (update-thunk (cdr pair)))
          (set-car! hash (cons (cons key (init-thunk)) data)) ) ) ) )

(define (hash-table-values hash)
  (let ((data (car hash)))
    (reverse (map cdr data)) ) )

;;
;; SRFI-95 (sort)
;;

(define (map-pair proc list)
  (let loop ((list list)
             (result '()) )
    (cond ((null? list) (reverse result))
          ((null? (cdr list)) (reverse (cons (car list) result)))
          (else (loop (cddr list) (cons (proc (car list) (cadr list)) result))) ) ) )

(define (split the-list)
  (map list the-list) )

(define (merge list1 list2 <)
  (let loop ((list1 list1)
             (list2 list2)
             (result '()) )
    (cond ((null? list1) (append (reverse result) list2))
          ((null? list2) (append (reverse result) list1))
          ((< (car list1) (car list2))
           (loop (cdr list1) list2 (cons (car list1) result)) )
          (else
           (loop list1 (cdr list2) (cons (car list2) result)) ) ) ) )

(define (sort list <)
  (if (null? list) list
      (let ((merge (lambda (list1 list2) (merge list1 list2 <))))
        (let loop ((list (split list)))
          (if (null? (cdr list))
              (car list)
              (loop (map-pair merge list)) ) ) ) ) )

;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;
;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;

;;;
;;; Primitive objects
;;;

(define *undefined-slot-value* (list 'undefined))

(define (make-primitive class slot-count)
  (cons class (make-vector slot-count *undefined-slot-value*)) )

(define (make-primitive-<class> slot-count)
  (let ((primitive (cons '() (make-vector slot-count *undefined-slot-value*))))
    (set-car! primitive primitive)
    primitive ) )

(define (primitive? thing)
  (and (pair? thing) (pair? (car thing)) (vector? (cdr thing))) )

(define (primitive-class primitive)
  (car primitive) )

(define (primitive-ref primitive nth)
  (vector-ref (cdr primitive) nth) )

(define (primitive-set! primitive nth value)
  (vector-set! (cdr primitive) nth value) )

(define (undefined-slot-value? value)
  (eq? *undefined-slot-value* value) )

;;;
;;; Callable objects
;;;

(define *callables* (make-hash-table eq?))

(define (callable? procedure)
  (hash-table-exists? *callables* procedure) )

(define (object-of procedure)
  (if (procedure? procedure)
      (hash-table-ref *callables* procedure)
      procedure ) )

(define (add-callable! procedure object)
  (hash-table-set! *callables* procedure object) )

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
;;; Class layout bootstrapping declarations
;;;

;; Metaobject classes
(define <class>          (make-primitive-<class> 6))
(define <object>         (make-primitive <class> 6))
(define <slot>           (make-primitive <class> 6))
(define <effective-slot> (make-primitive <class> 6))
(define <generic>        (make-primitive <class> 6))
(define <method>         (make-primitive <class> 6))

;; Built-in classes
(define <procedure>        (make-primitive <class> 6))

;;;
;;; Class layout bootstrapping accessors
;;;

(define (object-class-ref              object) (primitive-class object))

(define (class-name-ref                 class) (primitive-ref class 0))
(define (class-direct-superclasses-ref  class) (primitive-ref class 1))
(define (class-direct-slots-ref         class) (primitive-ref class 2))
(define (class-all-superclasses-ref     class) (primitive-ref class 3))
(define (class-all-slots-ref            class) (primitive-ref class 4))
(define (class-instance-size-ref        class) (primitive-ref class 5))

(define (class-name-set!                class value) (primitive-set! class 0 value))
(define (class-direct-superclasses-set! class value) (primitive-set! class 1 value))
(define (class-direct-slots-set!        class value) (primitive-set! class 2 value))
(define (class-all-superclasses-set!    class value) (primitive-set! class 3 value))
(define (class-all-slots-set!           class value) (primitive-set! class 4 value))
(define (class-instance-size-set!       class value) (primitive-set! class 5 value))

(define (slot-name-ref                  slot) (primitive-ref slot 0))
(define (slot-init-keyword-ref          slot) (primitive-ref slot 1))
(define (slot-getter-ref                slot) (primitive-ref slot 2))
(define (slot-setter-ref                slot) (primitive-ref slot 3))

(define (slot-name-set!                 slot value) (primitive-set! slot 0 value))
(define (slot-init-keyword-set!         slot value) (primitive-set! slot 1 value))
(define (slot-getter-set!               slot value) (primitive-set! slot 2 value))
(define (slot-setter-set!               slot value) (primitive-set! slot 3 value))

(define (effective-slot-direct-getter-ref  slot) (primitive-ref slot 4))
(define (effective-slot-direct-setter-ref  slot) (primitive-ref slot 5))

(define (effective-slot-direct-getter-set! slot value) (primitive-set! slot 4 value))
(define (effective-slot-direct-setter-set! slot value) (primitive-set! slot 5 value))

;;;
;;; Classifier
;;;

;; Should be as fast as possible
(define (class-of object)
  (cond ((primitive? object) (object-class-ref object))
        ((callable?  object) (object-class-ref (object-of object)))
        ((procedure? object) <procedure>)
        (else (error "unsupported object type" object)) ) )

;;;
;;; Various utils
;;;

(define (for-each-initarg proc initargs)
  (unless (null? initargs)
    (proc (first initargs) (second initargs))
    (for-each-initarg proc (cddr initargs)) ) )

(define (for-each-with-index proc list)
  (let loop ((index 0)
             (list list) )
    (unless (null? list)
      (proc index (car list))
      (loop (+ 1 index) (cdr list)) ) ) )

;;;
;;; Class layout bootstrapping definitions
;;;

(define (make-slot . initargs)
  (let ((slot (make-primitive <slot> 4)))
    (initialize-slot! slot initargs)
    slot ) )

(define (make-effective-slot . initargs)
  (let ((effective-slot (make-primitive <effective-slot> 6)))
    (initialize-slot! effective-slot initargs)
    effective-slot ) )

(define (initialize-slot! slot initargs)
  (for-each-initarg
    (lambda (key value)
      (case key
        ((name:)         (slot-name-set!         slot value))
        ((init-keyword:) (slot-init-keyword-set! slot value))
        ((getter:)       (slot-getter-set!       slot value))
        ((setter:)       (slot-setter-set!       slot value))
        (else (error "unknown init keyword" "<slot>" key)) ) )
    initargs ) )

; Slots should go precisely in this order
; as direct accessors believe they do

(define direct-<object>-slots '())

(define direct-<class>-slots
  (list (make-slot 'name: 'name
           'init-keyword: 'name:
                 'getter:  name )

        (make-slot 'name: 'direct-superclasses
           'init-keyword: 'direct-superclasses:
                 'getter:  direct-superclasses )

        (make-slot 'name: 'direct-slots
           'init-keyword: 'direct-slots:
                 'getter:  direct-slots )

        (make-slot 'name: 'all-superclasses
                 'getter:  all-superclasses )

        (make-slot 'name: 'all-slots
                 'getter:  all-slots )

        (make-slot 'name: 'instance-size) ) )

(define direct-<slot>-slots
  (list (make-slot 'name: 'name
           'init-keyword: 'name:
                 'getter:  name )

        (make-slot 'name: 'init-keyword
           'init-keyword: 'init-keyword:
            'getter:       init-keyword )

        (make-slot 'name: 'getter
           'init-keyword: 'getter:
                 'getter:  getter )

        (make-slot 'name: 'setter
           'init-keyword: 'setter:
                 'getter:  setter ) ) )

(define direct-<effective-slot>-slots
  (list (make-slot 'name: 'direct-getter)
        (make-slot 'name: 'direct-setter) ) )

(define direct-<generic>-slots
  (list (make-slot 'name: 'name
           'init-keyword: 'name:
                 'getter:  name )

        (make-slot 'name: 'signature
           'init-keyword: 'signature:
                 'getter:  signature )

        (make-slot 'name: 'methods
                 'getter:  methods )

        (make-slot 'name: 'effective-function) ) )

(define direct-<method>-slots
  (list (make-slot 'name: 'discriminators
           'init-keyword: 'discriminators:
                 'getter:  discriminators )

        (make-slot 'name: 'method-body
           'init-keyword: 'method-body: ) ) )

(define direct-<procedure>-slots '())

(define all-<object>-slots         direct-<object>-slots)
(define all-<class>-slots          (append all-<object>-slots direct-<class>-slots))
(define all-<slot>-slots           (append all-<object>-slots direct-<slot>-slots))
(define all-<effective-slot>-slots (append all-<slot>-slots   direct-<effective-slot>-slots))
(define all-<procedure>-slots      (append all-<object>-slots direct-<procedure>-slots))
(define all-<generic>-slots        (append all-<object>-slots direct-<generic>-slots))
(define all-<method>-slots         (append all-<object>-slots direct-<method>-slots))

(define (all-slots-of class)
  (cond ((eq? class <object>)         all-<object>-slots)
        ((eq? class <class>)          all-<class>-slots)
        ((eq? class <slot>)           all-<slot>-slots)
        ((eq? class <effective-slot>) all-<effective-slot>-slots)
        ((eq? class <procedure>)      all-<procedure>-slots)
        ((eq? class <generic>)        all-<generic>-slots)
        ((eq? class <method>)         all-<method>-slots) ) )

(define (initialize-class! class . initargs)
  (for-each-initarg
    (lambda (key value)
      (case key
       ((name:)                (class-name-set!                class value))
       ((direct-superclasses:) (class-direct-superclasses-set! class value))
       ((direct-slots:)        (class-direct-slots-set!        class value))
       (else (error "unknown init keyword" "<class>" key)) ) )
    initargs )

  (class-all-superclasses-set! class
    (cdr (graph-bfs class class-direct-superclasses-ref eq?)) )

  (let ((all-slots
          (map (lambda (slot)
                 (make-effective-slot
                   'name:          (slot-name-ref         slot)
                   'init-keyword:  (slot-init-keyword-ref slot)
                   'getter:        (slot-getter-ref       slot)
                   'setter:        (slot-setter-ref       slot) ) )
            (all-slots-of class) ) ))

    (let ((make-getter (if (eq? class <generic>)
                           (lambda (index) (lambda (o) (primitive-ref (object-of o) index)))
                           (lambda (index) (lambda (o) (primitive-ref o index))) ))
          (make-setter (if (eq? class <generic>)
                           (lambda (index) (lambda (o v) (primitive-set! (object-of o) index v)))
                           (lambda (index) (lambda (o v) (primitive-set! o index v))) )) )
      (for-each-with-index
        (lambda (index slot)
          (effective-slot-direct-getter-set! slot (make-getter index))
          (effective-slot-direct-setter-set! slot (make-setter index)) )
        all-slots ) )

    (class-all-slots-set!     class all-slots)
    (class-instance-size-set! class (length all-slots)) ) )

;;;
;;; General utils
;;;

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
;;; Class layout bootstrapping actions
;;;

(initialize-class! <object>
  'name:                '<object>
  'direct-superclasses: '()
  'direct-slots:         direct-<object>-slots )

(initialize-class! <class>
  'name:               '<class>
  'direct-superclasses: (list <object>)
  'direct-slots:        direct-<class>-slots )

(initialize-class! <slot>
  'name:               '<slot>
  'direct-superclasses: (list <object>)
  'direct-slots:        direct-<slot>-slots )

(initialize-class! <effective-slot>
  'name:               '<effective-slot>
  'direct-superclasses: (list <slot>)
  'direct-slots:        direct-<effective-slot>-slots )

(initialize-class! <procedure>
  'name:               '<procedure>
  'direct-superclasses: (list <object>)
  'direct-slots:        direct-<procedure>-slots )

(initialize-class! <generic>
  'name:               '<generic>
  'direct-superclasses: (list <procedure>)
  'direct-slots:        direct-<generic>-slots )

(initialize-class! <method>
  'name:               '<method>
  'direct-superclasses: (list <object>)
  'direct-slots:        direct-<method>-slots )

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
  (cond
    ((eq? (class-of object) <class>) (slot-ref-<class> object slot-name))
    ((eq? (class-of object) <effective-slot>) (slot-ref-<effective-slot> object slot-name))
    (else (slot-ref-<object> object slot-name)) ) )

(define (slot-ref-<class> class slot-name)
  (case slot-name
    ((name)                (class-name-ref                class))
    ((direct-superclasses) (class-direct-superclasses-ref class))
    ((direct-slots)        (class-direct-slots-ref        class))
    ((all-superclasses)    (class-all-superclasses-ref    class))
    ((all-slots)           (class-all-slots-ref           class))
    ((instance-size)       (class-instance-size-ref       class))
    (else (error "unknown slot" "<class>" slot-name)) ) )

(define (slot-ref-<effective-slot> slot slot-name)
  (case slot-name
       ((name)          (slot-name-ref                    slot))
       ((init-keyword)  (slot-init-keyword-ref            slot))
       ((getter)        (slot-getter-ref                  slot))
       ((setter)        (slot-setter-ref                  slot))
       ((direct-getter) (effective-slot-direct-getter-ref slot))
       ((direct-setter) (effective-slot-direct-setter-ref slot))
       (else (error "unknown slot" "<effective-slot>" slot-name)) ) )

(define (slot-ref-<object> object slot-name)
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

      (let ((make-getter (if (subclass? class <procedure>)
                             (lambda (index) (lambda (o) (primitive-ref (object-of o) index)))
                             (lambda (index) (lambda (o) (primitive-ref o index))) ))
            (make-setter (if (subclass? class <procedure>)
                             (lambda (index) (lambda (o v) (primitive-set! (object-of o) index v)))
                             (lambda (index) (lambda (o v) (primitive-set! o index v))) )) )
        (for-each-with-index
          (lambda (index slot)
            (set-direct-getter! slot (make-getter index))
            (set-direct-setter! slot (make-setter index)) )
          all-slots ) )

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
