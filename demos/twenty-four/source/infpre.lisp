;;;; Common Lisp infix/prefix conversion utility
;;;
;;;  $Id: infpre.lisp,v 1.2 2006/11/19 16:01:52 jornv Exp $
;;;
;;;; Licence LGPL
;;;
;;;; Copyright: Joern Inge Vestgaarden (jivestgarden at gmail com)
;;; 
;;;; Syntax: 
;;;   Works directly on lisp lists, not on strings.
;;;   The cost is that all operators must be separated by spaces, 
;;;   i.e. 1 + 2, not 1+2. 
;;;   
;;;   Unlike most infix utilities, the infix conversion
;;;   does not interpret +,*, etc. as binary operators,
;;;   but as list separted by the operator
;;;   i.e. (1 + 2 + 3) -> (+ 1 2 3) not (+ (+ 1 2) 3). 
;;;
;;;   The order of the operators determine precedence. 
;;;
;;;; Examples:
;;;   (1 + 2 * exp (-1 * x) * 3) -> (+ 1 (* 2 (exp (* -1 x)) 3))
;;; 
;;;; Bugs: 
;;;   Works directly on CL symbols which cause problems with packages.
;;;   The math macro only works because +-*/ are speical variables
;;;   in the common-lisp package. In general a new test-function
;;;   working on names must be made and supplied.
;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :infpre
    (:use :common-lisp)
    (:export #:infix->prefix
	     #:prefix->infix
	     #:math
	     #:!!)
    (:documentation "")))

(in-package :infpre)

(defvar *separators* (list '+ '- '* '/) "Default operators for the math macro") 

(defun remove-brackets (lst)
  "Reduses lists with just one item to the item itself"
  (do ((result lst (car result)))
      ((or (not (consp result))
	   (not (null (cdr result)))) result)))

(defun separate-list (lst separator test)
  "Returns list of sub-sequences defined by separator"
  (if (not (consp lst))
      lst
      (let ((result (cons separator nil)) (end 0) (sub)
	    (lst (if (funcall test (car lst) separator)
		     (cdr lst)
		     lst)))
	(do () ((null lst) result)
	  (setf end 
		(position separator lst :test test))
	  (setf sub
		(cons (subseq lst 0 end) nil))
	  (setf result 
		(append result sub))
	  (setf lst 
		(if end 
		    (nthcdr (+ 1 end) lst)
		    nil)))
	(setf (cdr result) (mapcar #'remove-brackets (cdr result)))
	result)))

(defun separate-tree (lst separator test)
  "Apply separate-list on all sublists"
  (if (or (not (consp lst)) (eql (first lst) 'quote))
      lst
      (progn
	(setf lst (mapcar #'(lambda (x) 
			      (if (not (consp x))
				  x
				  (separate-tree x separator test)))
			  lst))
	(if (not (find separator (rest lst)))
	    lst
	    (separate-list lst separator test)))))

(defun infix->prefix (infix-expr separators &key (test #'eql))
  "Converts an infix expression to prefix"
  (let ((result infix-expr))
    (dolist (sep separators)
      (setf result (separate-tree result sep test)))
    (remove-brackets result)))

(defun insert-between (lst sep)
  (if (or (not (consp lst))
	  (not (rest lst)))
      lst
    (cons (first lst) (mapcan #'(lambda (x) (list sep x)) (rest lst)))))

(defun prefix->infix (prefix-expr separators &key (test #'eql))
  "Converts a prefix expression to infix"
  (let ((in-expr (mapcar #'(lambda (x)
			     (remove-brackets (if (listp x)
					      (prefix->infix x separators)
					    x)))
			 prefix-expr)))
    (if (or (not (listp in-expr))
	    (not (member (first in-expr) separators :test test)))
	in-expr
      (insert-between (rest in-expr) (first in-expr)))))


;;;; End of infix prefix conversion


;;;; Additional usefull macros as interfaces to infix->prefix

(defmacro !! (&body body)
  "Converts infix to prefix"
  (infix->prefix body *separators*))

(defmacro math (name args  &body body)
  "Similar to defun, only with infix math. If name is _ then make a lambda expression"
  (let* ((body2 (if (stringp (car body))
		    (infix->prefix (cdr body) *separators*)
		    (infix->prefix body *separators*)))
	 (doc    (if (stringp (car body))
		     (car body)
		     "Math function")))
    (if (eql name '_)
	(compile nil (list 'lambda args body2))
	(list 'defun name args doc body2))))


		