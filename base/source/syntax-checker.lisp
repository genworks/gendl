(in-package :gdl)

(defparameter *on-syntax-error* :warn "Keyword symbol. Can be either :warn or :error.")

(define-condition syntax-error (error)
  ((text :initarg :text :reader text)))

(defun syntax-error (name token message-key message)
  (let ((text (format nil "Syntax error for class ~s.~%Offending token: ~s~%~a: ~a." name token message-key message)))
    (ecase *on-syntax-error*
    (:warn (warn text))
    (:error (error 'syntax-error :text text)))))

(defun check-floating-string (name form)
  "check for special case in which documentation isn't followed by symbol spec"
  (when (stringp (first (last form)))
    (syntax-error name (first (last form)) "Problem" "documentation string isn't followed by any message symbol")))

(defun check-form (name form predicate grammar)
  "general function that, given a predicate, validates all tokens in a slot declaration form"
  (dolist (token form)
    (if (not (funcall predicate token))
	(syntax-error name token "Valid Grammar" grammar))))

(defun check-input-slots (name form)
  ":input-slots grammar:

   <form>     = :input-slots (<token>*)
   <token>    = <string> | <symbol> | (<string>* <symbol> <expression>+ <behavior>*)
   <behavior> = :settable | :defaulting

  Also check for special case in which only strings without a symbol following."
  (when form

    (let ((behavior '(:settable :defaulting))
	  (grammar "
<form>     = :input-slots (<token>*)
<token>    = <string> | <symbol> | (<string>* <symbol> <expression>+ <behavior>*)
<behavior> = :settable | :defaulting"))

    (flet ((valid-token (token)
	     (or (symbolp token)
		 (stringp token)
		 (and (consp token)
		      (let ((token (strip-strings token)))
			(and (symbolp (first token))
			     ;; RvD: this check still allows duplicate keys in tail of cons
			     (every
			      #'(lambda (token*) (member token* behavior))
			      (rest (rest token)))))))))

	(check-form name form #'valid-token grammar)
	(check-floating-string name form)	
	))))

(defun check-computed-slots (name form)
  ":computed-slots grammar:

   <form>     = :computed-slots (<token>*)
   <token>    = <string> | (<string>* <symbol> <expression>+ <behavior>+)
   <behavior> = :settable | :uncached

  Also check for special case in which only strings without a symbol following."
  (when form

    (let ((behavior '(:settable :uncached))
	  (grammar "
<form>     = :computed-slots (<token>*)
<token>    = <string> | (<string>* <symbol> <expression>+ <behavior>*)
<behavior> = :settable | :uncached"))

      (flet ((valid-token (token)
	       (or (stringp token)
		   (and (consp token)
			(let ((token (strip-strings token)))
			  (and (symbolp (first token))
			       ;; RvD: unlike :input-slots, :computed-slots only allows one behavior keyword
			       ;; RvD: otherwise GDL compiler throws warnings.
			       (or (not (rest (rest token)))
				   (and (= (length token) 3)
					(member (third token) behavior)))
			       ))))))
				
	(check-form name form #'valid-token grammar)
	(check-floating-string name form)
	))))

(defun check-objects (name form &optional (hidden nil))
  ":(hidden-)objects grammar:

   <form>  = (hidden-):objects (<token>*)
   <token> = (<string>* <symbol> <rule>*)
   <rule>  = <keyword> <expression>"

  (when form
    (let ((grammar (format nil "
<form>  = :~aobjects (<token>*)
<token> = (<string>* <symbol> <rule>*)
<rule>  = <keyword> <expression>" (if hidden "hidden-" ""))))
      (flet ((valid-token (token)
	       (and (consp token)
		    (let ((token (strip-strings token)))
		      (and (symbolp (first token))
			   ;; plist is already checked by define-object internals, but do it here anyway
			   (or (and (evenp (length (rest token)))
				    (every #'keywordp (plist-keys (rest token))))
			       (syntax-error name (rest token) "Problem" (format nil "Unvalid plist for slot ~s" (first token)))
			       t)
			   )))))
	(check-form name form #'valid-token grammar)
	))))

(defun check-functions (name form &optional (methods t))
  ":functions|:methods grammar:

   <form>  = :functions|:methods (<token>*)
   <token> = (<string>* <symbol> <behavior>+ <list> <body>)
   <behavior> = :cached | :cached-eql | :cached-= | :cached-eq | :cached-equal | :cached-equalp"

  (when form
    (let ((grammar (format nil "
<form>  = :~a (<token>*)
<token> = (<string>* <symbol> <behavior>+ <list> <body>*)
<behavior> = :cached | :cached-eql | :cached-= | :cached-eq | :cached-equal | :cached-equalp"
			   (if methods "methods" "functions")))
	  (behavior '(:cached :cached-eql :cached-= :cached-eq :cached-equal :cached-equalp)))
      (flet ((valid-token (token)
	       (and (consp token)
		    (let ((token (strip-strings token)))
		      (and (symbolp (first token))
			   (or (listp (second token))
			       (and (member (second token) behavior)
				    (listp (third token))))
			   )))))
      
	(check-form name form #'valid-token grammar)
	))))

(defun check-trickle-down-slots (name form)
  ":functions|:methods grammar:

   <form>  = :trickle-down-slots (<symbol>*)
"
  (when form
    (let ((grammar "<form>  = :trickle-down-slots (<symbol>*)"))
      (if (not (and (consp form)
		    (every #'symbolp form)))
	  (syntax-error name form "Valid Grammar" grammar)))))

(defun check-query-slots (name form)
  "unknown what is is"
  (declare (ignore name form))
  t)

(defun check-documentation (name form)
  "plist containing keys :description :author :examples :date :version"
  (when form
    (let ((valid-keys '(:description :author :examples :date :version)))
      (when (not (and (consp form)
		      (evenp (length form))
		      (every #'(lambda (key) (member key valid-keys)) (plist-keys form))))
	(syntax-error name form "Problem"
		      (format nil ":documentation is a plist which may contain the following keys: ~a" valid-keys))))))

(defun check-syntax (name
		     input-slots
		     computed-slots
		     objects
		     hidden-objects
		     functions
		     methods
		     documentation
		     trickle-down-slots
		     query-slots)
  (check-input-slots name input-slots)
  (check-computed-slots name computed-slots)
  (check-objects name objects)
  (check-objects name hidden-objects t)
  (check-functions name functions)
  (check-functions name methods t)
  (check-trickle-down-slots name trickle-down-slots)
  (check-query-slots name query-slots)
  (check-documentation name documentation)
  )
