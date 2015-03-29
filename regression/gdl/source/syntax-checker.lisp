(gwl:define-package :gdl-lift-tests)

(in-package :gdl-lift-tests)

(define-condition not-raised (error)
  ;; Private condition to use inside assert-raises."
  ((expected-condition :initarg :expected-condition :reader expected-condition)
   (raised-condition :initarg :raised-condition :reader raised-condition :initform nil)
   (expression :initarg :expression :reader expression)
   (msg :initarg :msg :reader msg))
  (:report (lambda (condition stream)
             (format stream "AssertionError: condition not raised.
Message: ~a
Expression: ~s
Expected: '~s
Raised: ~s
Print: ~a"
		     (msg condition)
		     (expression condition)		     
		     (expected-condition condition)
		     (raised-condition condition)
		     (raised-condition condition))
	     )))

(define-condition was-raised (error)
  ;; Private condition to use inside assert-raises."
  ((raised-condition :initarg :raised-condition :reader raised-condition)
   (expression :initarg :expression :reader expression)
   (msg :initarg :msg :reader msg))
  (:report (lambda (condition stream)
             (format stream "AssertionError: condition was raised.
Message: ~a
Expression: ~s
Raised: ~s
Print: ~a"
		     (msg condition)
		     (expression condition)
		     (raised-condition condition)
		     (raised-condition condition)
                     ))))

(defmacro assert-raises (condition quoted msg)
  "Custom test for quoted `define-object expresssion. Check that condition is raised"
  `(handler-case
       (progn (eval ,quoted)
	      ;; will be caught and re-raised
	      (error 'not-raised))
     (not-raised () (error 'not-raised
			   :expected-condition ',condition
			   :expression ,quoted
			   :msg ,msg))
     (,condition () t)
     (error (e) (error 'not-raised
		       :expected-condition ',condition
		       :raised-condition e
		       :expression ,quoted
		       :msg ,msg))
     ))

(defmacro assert-not-raises (quoted msg)
  "Custom test for quoted `define-object expresssion. Check that not condition is raised"
  `(handler-case
       (eval ,quoted)
     (error (e) (error 'was-raised :raised-condition e :expression ,quoted :msg ,msg))))

(defmacro defobj (key &rest body)
  "Make a quick define-object code for testing."
  ;; still return quoted, we eval later
  `(quote
    (define-object foo ()
      ,key
      (,@body))))


(define-object syntax-error-test ()
  ;; it's important that syntax-checking code and patches to define-object macros are loaded first!!!
  :computed-slots
  ((regression-test-data
    (let ((gdl::*on-syntax-error* :error))

      ;; ############
      ;; :input-slots
      ;; ############
      (assert-not-raises (defobj :input-slots sym) "simple symbol")
      (assert-not-raises (defobj :input-slots "doc" sym) "docstring followed by symbol")
      (assert-not-raises (defobj :input-slots ("doc" sym)) "docstring followed by symbol")
      (assert-not-raises (defobj :input-slots ("doc" sym 2)) "with default")
      (assert-not-raises (defobj :input-slots ("doc" sym 2 :defaulting :settable)) "with behavior")

      (assert-raises gdl::syntax-error
		     (defobj :input-slots 2)
		     "Invalid token")
      (assert-raises gdl::syntax-error
		     (defobj :input-slots (1 sym))
		     "Invalid doc token")
      (assert-raises gdl::syntax-error
		     (defobj :input-slots ("doc" 1 sym))
		     "Invalid second doc token")
      (assert-raises gdl::syntax-error
		     (defobj :input-slots (sym 1 :settable :nonsense))
		     "Invalid behavior")
      (assert-raises gdl::syntax-error
		     (defobj :input-slots sym "free-floating")
		     "free-floating string")

      ;; ###############
      ;; :computed-slots
      ;; ###############
      (assert-not-raises (defobj :computed-slots "doc" (sym)) "docstring followed by symbol")
      (assert-not-raises (defobj :computed-slots ("doc" sym)) "docstring followed by symbol")
      (assert-not-raises (defobj :computed-slots ("doc" sym 2)) "with default")
      (assert-not-raises (defobj :computed-slots ("doc" sym 2 :settable)) "with behavior")

      (assert-raises gdl::syntax-error
		     (defobj :computed-slots 2)
		     "Invalid token")
      (assert-raises gdl::syntax-error
		     (defobj :computed-slots sym)
		     "Symbol without default")
      (assert-raises gdl::syntax-error
		     (defobj :computed-slots (1 sym))
		     "Invalid doc token")
      (assert-raises gdl::syntax-error
		     (defobj :computed-slots ("doc" 1 sym))
		     "Invalid second doc token")
      (assert-raises gdl::syntax-error
		     (defobj :computed-slots (sym 1 :nonsense))
		     "Invalid behavior")
      (assert-raises gdl::syntax-error
		     (defobj :computed-slots sym "free-floating")
		     "free-floating string")

      ;; ########
      ;; :objects
      ;; ########
      (assert-not-raises (defobj :objects ("doc" sym :type 'bar)) "valid :objects grammar")

      (assert-raises gdl::syntax-error
		     (defobj :objects "doc")
		     "invalid docstring")
      (assert-raises gdl::syntax-error
		     (defobj :objects sym)
		     "invalid symbol")
      (assert-raises gdl::syntax-error
		     (defobj :objects 1)
		     "invalid symbol")
      (assert-raises gdl::syntax-error
		     (defobj :objects (1 sym))
		     "invalid docstring")
      (assert-raises gdl::syntax-error
		     (defobj :objects ("doc" sym 1))
		     "invalid plist")

      ;; ##########
      ;; :functions
      ;; ##########
      (assert-not-raises (defobj :functions ("doc" sym :cached (bar) (+ bar 1))) "valid :functions grammar")
  
  
      (assert-raises gdl::syntax-error
		     (defobj :functions "doc")
		     "invalid docstring")
      (assert-raises gdl::syntax-error
		     (defobj :functions sym)
		     "invalid symbol position")
      (assert-raises gdl::syntax-error
		     (defobj :functions 1)
		     "invalid token")
      (assert-raises gdl::syntax-error
		     (defobj :functions (1 sym))
		     "invalid doctring")  
      (assert-raises gdl::syntax-error
		     (defobj :functions ("doc" sym :nonsense))
		     "invalid behavior")  
      (assert-raises gdl::syntax-error
		     (defobj :functions ("doc" sym 2))
		     "invalid parameter list")

      ;; ###################
      ;; :trickle-down-slots
      ;; ###################
      (assert-not-raises (defobj :trickle-down-slots ) "valid :trickle-down-slots grammar")
      (assert-not-raises (defobj :trickle-down-slots bar baz) "valid :trickle-down-slots grammar")

      (assert-raises gdl::syntax-error 
		     (defobj :trickle-down-slots 1 2)
		     "invalid tokens")
      (assert-raises gdl::syntax-error 
		     (defobj :trickle-down-slots (sym) sym)
		     "invalid nesting")
  
      ;; ##############
      ;; :documentation
      ;; ##############
      (assert-not-raises (defobj :documentation ) "valid :documentation")
      (assert-not-raises (defobj :documentation :author "Reinier van Dijk" :description "some test") "valid :documentation")
  
      (assert-raises gdl::syntax-error 
		     (defobj :documentation 1)
		     "invalid plist")
      (assert-raises gdl::syntax-error 
		     (defobj :documentation :nonsense 1)
		     "invalid keywords")
      ))))


;; (register-test-definition 'syntax-error-test)
