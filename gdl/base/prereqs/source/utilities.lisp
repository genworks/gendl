;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 

(in-package :gdl)

(defun plist2pairs (plist)
  (when plist
    (append (list (list (first plist)
                        (second plist)))
            (plist2pairs (rest (rest plist))))))

(defun plist-keys (plist)
  "List of keyword symbols. Returns the keys from a plist.

:arguments (plist \"Plist\")"
  (when plist
    (cons (first plist)
          (plist-keys (rest (rest plist))))))

(defun plist-values (plist)
  "List of Lisp objects. Returns the values from a plist.

:arguments (plist \"Plist\")"
  (when plist
    (cons (second plist)
          (plist-values (rest (rest plist))))))

(defun remove-plist-key (plist key)
  (let (result on-remark?)
    (dolist (element plist (nreverse result))
      (cond ((eql element key)
             (setq on-remark? t))
            (on-remark?
             (setq on-remark? nil))
            (t (push element result))))))

(defun remove-plist-keys (plist keys)
  (if (null keys)
      plist
    (when plist
      (remove-plist-keys (remove-plist-key plist (first keys)) (rest keys)))))


(defmacro print-variables (&rest vars)
  "[Macro] Void. Prints the specified variables and current values to standard output.

:arguments (vars \"unquoted symbols (&rest argument)\")"

  `(let (*print-readably*)
     (format *trace-output* 
           ,(with-output-to-string(ss)
              (write-string "~&" ss)
              (mapcar #'(lambda(var)
                          (format ss "~&~a = ~~s~%" var)) vars)
              (write-string "~%" ss))
           ,@vars)))

(defmacro print-messages (&rest messages)
  "[Macro] Void. Prints the specified GDL object messages (i.e. slots) and their current values to standard output.

:arguments (vars \"unquoted symbols  (&rest argument)\")"
  `(let (,@(mapcar #'(lambda(message)
                       `(,message (the ,message))) messages))
     (print-variables ,@messages)))

(defun string-append (&rest args)
  "String. Returns a new string made up of concatenating the arguments.
:arguments 
 (:&rest (args \"strings\"))
"
  
  (apply #'concatenate 'string args))


(defun make-keyword (&rest strings)
  "Keyword symbol. Converts given strings to a keyword.
If any of the given arguments is not a string, it will be converted to one with 
 (format nil \"~a\" string).

:arguments (strings \"&rest Strings\")"
  (%make-keyword (apply #'string-append (mapcar #'(lambda(string) (string (%make-keyword string))) strings))))
  

(defun make-keyword-sensitive (string) (glisp:intern string :keyword))


(defun %make-keyword (string)

  (when (not (stringp string)) (setq string (format nil "~a" string)))
  
  (when (and (not (zerop (length string)))
             (eql (aref string 0) #\:))
    (setq string (subseq string 1)))
  
  (multiple-value-bind (symbol flag)
      (glisp:intern (glisp:upcase string) :keyword)
    (declare (ignore flag))
    symbol))

(defun ensure-list (possible-list)
  "List. If argument is not list, returns it in a list. If argument is
a list, returns it unchanged.

:arguments (possible-list \"Lisp object\")"
  (if (listp possible-list) 
      possible-list
    (list possible-list)))


(defun all-superclasses (class)
  (let ((direct-supers (glisp:direct-superclasses class)))
    (remove-duplicates
     (append direct-supers
             (mapcan #'(lambda(class)
                         (all-superclasses class)) direct-supers)))))

(defun compare-items (new-cons old-cons)
  (destructuring-bind (node1 . message1) new-cons
    (destructuring-bind (node2 . message2) old-cons
      (and (eq node1 node2) (eq message1 message2)))))


(defparameter *dependency-hash* (glisp:make-weak-hash-table))


(defparameter *run-with-dependency-recording?* nil)
(defparameter *compile-with-dependency-recording?* t)


(defun find-messages-used-by (instance message-key)
  "List of pairs of instance/keyword. This returns the list of direct dependants of a given 
message in a given instance. Note that this is not recursive; if you want to generate a tree, 
then you have to call this recursively yourself.

If you want an easy way to remember the meaning of dependant and dependency:

You have a dependency on caffeine. Your children are your dependants.

"
  (let ((message-hash (gethash instance *dependency-hash*)))
    (when message-hash (reverse (gethash message-key message-hash)))))

(defun find-dependants (instance message-key)
  (find-messages-which-use instance message-key))

(defun find-messages-which-use (instance message-key)
  "List of pairs of instance/keyword. This returns the list of direct dependants of a given 
message in a given instance. Note that this is not recursive; if you want to generate a tree, 
then you have to call this recursively yourself.

If you want an easy way to remember the meaning of dependant and dependency:

You have a dependency on caffeine. Your children are your dependants.

"
  (let ((notify-cons (second (slot-value instance (glisp:intern (string message-key) :gdl-acc)))))
    ;;
    ;; FLAG -- might have to normalize this into instance/message
    ;; pairs if messages are grouped on single instance
    ;;
    (let (result)
      (dolist (group notify-cons (nreverse result))
        (let ((instance (first group)))
          (dolist (message (rest group)) 
            (push (list instance (make-keyword (string message))) result)))))))

(defun find-dependencies (instance message-key)
  (find-messages-used-by instance message-key))


(let ((value (gensym)) (need? (gensym)))
  (defmacro with-dependency-tracking ((message-symbol &optional (self-sym 'self)) &rest body)
    `(let ((,value (,message-symbol ,self-sym))
	   (,need? (and *run-with-dependency-tracking?* *notify-cons* *root-checking-enabled?*
			;;
			;; FLAG -- check that remotes are effectively from same tree as well
			;;
			(or (and (find-class 'gdl-remote nil) (typep (first *notify-cons*) 'gdl-remote))
			    (same-tree? ,self-sym (first *notify-cons*))))))
       
       
       ,(when *compile-circular-reference-detection?*
	  `(when (and *run-with-circular-reference-detection?*
		  (member (list ,self-sym ',message-symbol) *till-now* :test #'equalp))
	     (error "Circular reference detected")))
       
       (if (eq (first (ensure-list ,value)) 'gdl-rule::%unbound%)
	   (progn (setq ,value (list (let* ,(remove nil
					     (list (when *compile-dependency-tracking?*
						     `(*notify-cons* (when *run-with-dependency-tracking?* 
								       (list ,self-sym ',message-symbol))))
						   (when *compile-circular-reference-detection?*
						     `(*till-now* (when *run-with-circular-reference-detection?*
								    (cons (list ,self-sym ',message-symbol) 
									  *till-now*))))))
				       ,@body)
				     (when ,need? (list (copy-list *notify-cons*)))))
		  (if (not (eql (first ,value) 'gdl-rule:%not-handled%)) 
		      (setf (,message-symbol ,self-sym) ,value)
		      ;;(without-interrupts (setf (,message-symbol ,self-sym) ,value))
		    
		    (progn (setq ,need? nil)
			   (not-handled ,self-sym ',message-symbol))))
	 (when ,need? (add-notify-cons *notify-cons* ,value)))
       
       (first ,value))))


(defun add-notify-cons (notify-cons value)
  (let ((matching-sublist (assoc (first notify-cons) (second value))))
    (if matching-sublist (pushnew (second notify-cons) (rest matching-sublist))
      (push (copy-list notify-cons) (second value)))))


;;
;; FLAG -- merge the following two back in when we have adaptive Abstract Associative Map. 
;;
#+nil
(let ((value (gensym)) (need? (gensym)))
  (defmacro with-dependency-tracking ((message-symbol &optional (self-sym 'self)) &rest body)
    `(let ((,value (,message-symbol ,self-sym))
           (,need? (and *run-with-dependency-tracking?* 
                        *notify-cons* *root-checking-enabled?*)))
       ,(when *compile-circular-reference-detection?*
          `(when (and *run-with-circular-reference-detection?*
                  (member (list ,self-sym ',message-symbol) *till-now* :test #'equalp))
             (error "Circular reference detected")))

       ,(when *compile-with-dependency-recording?*
          `(when (and ,*run-with-dependency-recording?*
                      *notify-cons*)
             (let ((current-hash (gethash (first *notify-cons*) *dependency-hash*)))
               (unless current-hash 
                 (setf (gethash (first *notify-cons*) *dependency-hash*)
                   (make-hash-table)))
               (pushnew (list ,self-sym ,(make-keyword message-symbol))
                        (gethash (make-keyword (second *notify-cons*))
                                 (gethash (first *notify-cons*) *dependency-hash*))
                        :test #'equalp))))
       
       (when (eq (first (ensure-list ,value)) 'gdl-rule::%unbound%)
         (progn (setq ,value (list (let* ,(remove nil
                                           (list (when *compile-dependency-tracking?*
                                                   `(*notify-cons* (when *run-with-dependency-tracking?* 
                                                                     (list ,self-sym ',message-symbol))))
                                                 (when *compile-circular-reference-detection?*
                                                   `(*till-now* (when *run-with-circular-reference-detection?*
                                                                  (cons (list ,self-sym ',message-symbol) 
                                                                        *till-now*))))))
                                       
                                     ,@body)
                                   nil))
                (if (not (eql (first ,value) 'gdl-rule:%not-handled%)) 
                    (setf (,message-symbol ,self-sym) ,value)
                  (progn (setq ,need? nil)
                         (not-handled ,self-sym ',message-symbol)))))
       
       (when ,need? (add-notify-key *notify-cons* ,value))
       
       (first ,value))))

#+nil
(defun add-notify-key (notify-cons value)
  (if (second value)
      (pushnew (second notify-cons) (gethash (first notify-cons) (second value)))
    (setf (second value)
      (let ((ht (make-hash-table)))
        (setf (gethash (first notify-cons) ht) (list (second notify-cons)))
        ht))))



(defun same-tree? (obj1 obj2) (eql (gdl-acc::%root% obj1) (gdl-acc::%root% obj2)))
    
(defun merge-common-keys (plist)
  (let ((ht (make-hash-table)) result)
    (mapc #'(lambda(key value) (let ((current (gethash key ht)))
                                 (setf (gethash key ht) (if current (append current value) value))))
          (plist-keys plist) (plist-values plist))
    (maphash #'(lambda(key value) (push key result) (push value result)) ht) (nreverse result)))


(defclass gdl-basis () () (:metaclass gdl-class))

(defun to-double-float (num) (coerce num 'double-float))
(defun to-single-float (num) (coerce num 'single-float))


(defvar *patch-fasls* nil)





