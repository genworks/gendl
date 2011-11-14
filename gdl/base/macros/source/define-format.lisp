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

(defmacro define-format (name mixin-list &key slots functions documentation)
  "Standard-class [Macro]. Defines a standard GDL output format for use with GDL views.

:arguments (name \"Symbol.\"
            mixin-list \"List of symbols.\")
:&key (documentation \"Plist containing keys and strings for author, description, etc.\"
       slots \"List of lists or symbols. If a list, the list should contain a symbol, a default value, and optionally a documentation string. If
a symbol, this is the name of the slot and there will be no default value.\"
       functions \"List of format-function definitions. Each definition is made up of a symbol, an argument-list, and a body.\")"
  
  (let ((old-format-functions (gensym))
        (new-format-functions (gensym))
        (class (gensym)))
    `(progn 
       (defclass ,name ,mixin-list ,(mapcar #'(lambda(slot)
                                                (if (atom slot)
                                                    `(,(glisp:intern (symbol-name slot) :gdl-acc) :initarg ,(make-keyword slot))
                                                  `(,(glisp:intern (symbol-name (first slot)) :gdl-acc) :initform ,(second slot) 
                                                                                                  :initarg ,(make-keyword (first slot))
                                                                                                  :documentation ,(third slot))))
                                            slots) (:metaclass gdl-format-class))
       (let ((,class (find-class ',name)))
         (let ((,old-format-functions (format-functions ,class))
               (,new-format-functions ',(mapcar #'first-symbol (remove nil functions))))
           (dolist (key (set-difference ,old-format-functions ,new-format-functions))
             (let ((method (ignore-errors (find-method (symbol-function (glisp:intern (symbol-name key) :gdl-format)) nil
                                                       (list (find-class ',name)) nil))))
               (when method 
                 (format t "~%Removing format function: ~a for format definition: ~s~%" key ',name)
                 (remove-method (symbol-function (glisp:intern (symbol-name key) :gdl-format)) method))))
           (setf (format-functions ,class) ,new-format-functions)
           (setf (gdl-documentation ,class) ',documentation)))
       ,@(format-functions-section name (remove nil functions))
       
       (find-class ',name)
       
       )))



(defun format-functions-section (name methods)
  (mapcar 
   #'(lambda(method)
       (let* ((attr-sym (first method))
              (lambda-list (second method))
              (body (rest (rest method))))
         `(defmethod ,(glisp:intern (symbol-name attr-sym) :gdl-format) ((format ,name) &rest ,args-arg)
            (apply #'(lambda ,lambda-list ,@body) ,args-arg))))
   methods))


(defmacro define-lens (format-and-object mixin-lists &key (skin t) output-functions amend? documentation)
  "Void [Macro]. Defines output-functions for the combination of the given output-format and GDL object. 

:arguments (format-and-object \"List of two symbols. The first should name an output-format previously 
defined with <tt>define-format</tt>, and the second should name a GDL object previously defined with <tt>define-object</tt>.\"
            mixin-list \"NIL. This is not supported and should be left as NIL or an empty list for now.\")
:&key ((skin t) \"Name of a skin defined with define-skin. This allows a class hierarchy of look and feel for each view combination. Defaults to T, a generic skin.\")
      (output-functions \"List of format-function definitions. Each definition is made up of a symbol, an argument-list, and a body. The
code in the body may refer to the dynamically bound variable <tt>stream</tt> for writing output.\")"
  (declare (ignore mixin-lists)) ;; FLAG -- might use mixins for new derived (name-mangled) class for holding attributes
  
  ;;
  ;; FLAG -- include skin in keeping track of function names
  ;;
  
  (let ((old-output-functions (gensym))
        (new-output-functions (gensym))
        (format-class (gensym))
        (object-class (gensym))
        (skin-class (gensym))
        (remote? (or (eql (second format-and-object) 'remote-object)
                     (eql (second format-and-object) 'gdl-remote))))
    `(progn 
       (let ((,format-class (find-class ',(first format-and-object)))
             (,object-class (find-class ',(second format-and-object)))
             (,skin-class (find-class ',skin)))
         (let ((,old-output-functions (let ((ht (gethash ,object-class (output-functions ,format-class))))
                                        (when ht (gethash ,skin-class ht))))
               (,new-output-functions ',(mapcar #'first-symbol (remove nil output-functions))))
           ,(when (not (or amend? remote?))
              `(dolist (key (set-difference ,old-output-functions ,new-output-functions))
                 (let ((method (ignore-errors 
                                (find-method (symbol-function (glisp:intern (symbol-name key) :gdl-output)) nil
                                             (list (find-class ',(first format-and-object))
                                                   (find-class ',(second format-and-object))
                                                   (find-class ',skin)) nil))))
                   (when method 
                     (format t "
Removing output function: ~a for view of format: ~a on object definition: ~s with skin: ~s~%" 
                             key 
                             ',(first format-and-object)
                             ',(second format-and-object) ',skin)
                     (remove-method (symbol-function (glisp:intern (symbol-name key) :gdl-output)) method)))))

           (let ((current (gethash ,object-class (output-functions ,format-class))))
             (unless current (setf (gethash ,object-class (output-functions ,format-class)) (make-hash-table))))
           
           (setf (gethash ,skin-class (gethash ,object-class (output-functions ,format-class)))
             ,(if amend? `(remove-duplicates (append ,old-output-functions ,new-output-functions))
                new-output-functions))
           
           (let ((current (gethash ,object-class (view-documentation ,format-class))))
             (unless current (setf (gethash ,object-class (view-documentation ,format-class)) (make-hash-table))))
           
           (setf (gethash ,skin-class (gethash ,object-class (view-documentation ,format-class)))
             ',documentation)))
  
       ,@(view-output-functions-section (first format-and-object) (second format-and-object) skin (remove nil output-functions) :remote? remote?))))



(defmacro define-view (&rest args)
  (warn "The name define-view is deprecated, please use define-lens instead.")
  `(define-lens ,@args))


(defun view-output-functions-section (format object-type-name skin format-functions &key remote?)
  ;;
  ;; FLAG -- consider reversing the order of args format and object-type
  ;;
  (mapcar 
   #'(lambda(function)
       (view-output-function format object-type-name skin function :remote? remote?)) format-functions))

(defun view-output-function (format object-type-name skin function &key remote?)
  (let* ((attr-remarks (message-strings function))
         (attr-sym (glisp:intern (symbol-name (message-symbol function)) :gdl-output))
         (lambda-list (second (strip-strings function)))
         (body (rest (rest (strip-strings function))))
         (has-declare? (and (consp body) (consp (first body))
                            (eql (first (first body)) 'declare))))
    (let ((method (gensym)))
      ;;
      ;; FLAG -- do we need this `remove nil' ?
      ;;
      (remove nil
              `(let ((,method (defmethod ,attr-sym ((format ,format) (self ,object-type-name) (skin ,skin) &rest ,args-arg)
                               ,(when (and *compile-documentation-database?* attr-remarks) attr-remarks)
                               (apply #'(lambda ,lambda-list
                                          ,(if has-declare? (first body) `(declare))
                                          (block ,(make-keyword attr-sym) ,@(if has-declare? (rest body) body))) ,args-arg))))
                 (declare (ignorable ,method))
                 (eval-when (compile load eval) (glisp:begin-redefinitions-ok))
                 ,(when (and *compile-for-dgdl?* (not remote?))
                    `(defmethod ,attr-sym ((format ,format) (self gdl-remote) (skin ,skin) &rest ,args-arg)
                       ,(when (and *compile-documentation-database?* attr-remarks) attr-remarks)
                       (funcall #'(lambda (&rest ,args-arg)
                                    ,(if has-declare? (first body) `(declare))
                                    (block ,(make-keyword attr-sym) 
                                      (the (send-output (:apply (cons ,(make-keyword attr-sym)
                                                                      (cons *%format%* ,args-arg))))))))))
                 (eval-when (compile load eval) (glisp:end-redefinitions-ok))
                 ,(when (and *compile-documentation-database?* attr-remarks)
                    `(when *load-documentation-database?* (setf (documentation ,method nil) ,attr-remarks))))))))


(defmacro define-skin (name &optional mixins slots)
  `(defclass ,name ,mixins ,slots (:metaclass gdl-skin-class)))


(defmacro defcompanion (&rest args)
  (declare (ignore args))
  (error "The Defcompanion translator is not loaded. Please load it with

    (require :defpart)

 and try compiling this code again.

 Also please see the file defpart.txt in the GDL documentation set for
 information on using the translator to convert legacy icad-style code."))





