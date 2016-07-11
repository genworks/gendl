;;
;; Copyright 2002-2011 Genworks International 
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

(defmacro the (&rest reference-chain)
  "Lisp object. Sends the <tt>reference-chain</tt> to <tt>self</tt>, which typically means it is 
used within the context of a define-object where self is automatically lexically bound.

:arguments (reference-chain \"(&rest). A spliced-in list of symbols naming messages, which can 
be slots or objects starting from <tt>self</tt>. For referring to elements of a quantified set,
or for passing arguments to GDL functions which take arguments, use parentheses around the
message name and enclose the quantified element index or function arguments after the message
name.\")

:example 
This example sends the <tt>length</tt> message to the ``zeroth'' element of the quantified
set of arms contained in the body which is contained in the robot which is contained in self:
<pre>
 (the robot body (arms 0) length)
</pre>"
  `(the-object self ,@reference-chain))


(defmacro the-object (object &rest reference-chain)
  "Lisp object. Sends the <tt>reference-chain</tt> to <tt>object</tt>, which must be specified
as a Lisp expression (e.g. a variable) which evaluates to a GDL object.

:arguments (reference-chain \"(&rest). A spliced-in list of symbols naming messages, which can 
be slots or objects starting from <tt>object</tt>. For referring to elements of a quantified set,
or for passing arguments to GDL functions which take arguments, use parentheses around the
message name and enclose the quantified element index or function arguments after the message
name.\")

:example 
This example sends the <tt>length</tt> message to the ``zeroth'' element of the quantified
set of arms contained in the body which is contained in the robot which is contained in <tt>object</tt>:
<pre>
 (the-object object robot body (arms 0) length)
</pre>"
  (if reference-chain
      (let ((first (first reference-chain)) evaluate? apply?)
        (let ((message (cond ((atom first) (glisp:intern (symbol-name first) :gdl-slots))
                             ((and (symbolp (first first))
                                   (not (eql (first first) 'evaluate))) (glisp:intern (symbol-name (first first)) :gdl-slots))
                             ((eql (first first) 'evaluate) (setq evaluate? t) (second first))
                             ((and (consp (first first))
                                   (eql (first (first first)) 'evaluate))
                              (setq evaluate? t)
                              (second (first first)))))
              (args (cond ((and (listp first) (eql (first first) 'evaluate)) nil)
                          ((and (listp first) (listp (first (rest first)))
                                (eql (first (first (rest first))) :apply))
                           (setq apply? t)(second (first (rest first))))
                          ((listp first) (rest first))))
              ;;(new-object (gensym))
              (new-object '+new-object+)
              )
          
          
          `(the-object ,(if *undeclared-parameters-enabled?*
                            (if evaluate?
                                `(if (fboundp (glisp:intern (symbol-name ,message) :gdl-slots))
                                     (funcall (symbol-function (glisp:intern (symbol-name ,message) :gdl-slots)) ,object ,@args)
                                     (let ((,new-object (getf (gdl-slots::%parameters% ,object) 
                                                              (make-keyword ,message) 'gdl-rule:%not-handled%)))
                                       (if (eql ,new-object 'gdl-rule:%not-handled%)
                                           (not-handled-error ,object ',message ',args) ,new-object)))
                                `(if (fboundp ',message) (,message ,object ,@args)
                                     (let ((,new-object (getf (gdl-slots::%parameters% ,object) 
                                                              ,(make-keyword message) 'gdl-rule:%not-handled%)))
                                       (if (eql ,new-object 'gdl-rule:%not-handled%)
                                           (not-handled-error ,object ',message ',args) ,new-object))))
                            (cond ((and evaluate? apply?)
                                   `(apply (symbol-function (glisp:intern (symbol-name ,message) :gdl-slots)) ,object 
                                           ,args))
                                  (evaluate?
                                   `(funcall (symbol-function (glisp:intern (symbol-name ,message) :gdl-slots)) ,object 
                                             ,@args))
                                  (apply?
                                   `(apply ',message ,object ,args))
                                
                                  (t `(,message ,object ,@args))))
                       
                       ,@(rest reference-chain))))
      object))



(defmacro the-child (&rest reference-chain)
  "similar to ``the,'' but used to refer to the child part from within an :objects or :hidden-objects specification.
This is often used for sending the <tt>index</tt> message to an element of a quantified set.

:arguments (reference-chain \"(&rest). A spliced-in list of symbols naming messages relative to the child object.\")"
  `(the-object child ,@reference-chain))


(defun not-handled-error (object message &optional args)
  (if (the-object object root?)
      (error "~s, which is the root object, could not handle the ~s message~a." 
	     object (make-keyword message)
	     (if args (format nil "~%with args: ~s " args) ""))
      (error "Neither ~s nor any of its ancestor instances could handle the ~s message~a
The path to the containing object is: ~s" 
	     object (make-keyword message) 
	     (if args (format nil "~%with args: ~s " args) "")
	     (append '(the :root) (symbols-as-keywords
				   (reverse (the-object object :root-path)))))))

(defmacro with-format ((format stream-or-file &rest args) &body body)
  "Void [Macro]. Used to establish an output format and a stream to which data is to be sent. This is the experimental
new version of the <tt>with-format</tt> macro, which supports a full range of output options such as page dimensions,
view transforms, view scales, etc.

:example
<pre>
 (gdl::with-format (pdf \"/tmp/box.pdf\" :view-transform (getf *standard-views* :trimetric)) 
    (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output))
</pre>"
  (let (;;(flag (gensym))
	(flag '+flag+)
	)
	
	
    `(let ((*%format%* (make-instance ',format ,@args)))
       (let ((*stream* (if (or (stringp ,stream-or-file) (pathnamep ,stream-or-file))
                         (open ,stream-or-file 
			       :if-does-not-exist (or (format-slot if-does-not-exist) :create)
			       :if-exists (or (format-slot if-exists) :error)
			       :direction (or (format-slot direction) :output)
			       :external-format (format-slot external-format)
			       :element-type (format-slot element-type))
                         ,stream-or-file))
             (,flag t))
         (unwind-protect
             (progn (multiple-value-prog1
                        ,(case format 
                           ;;
                           ;; NOTE -- most interesting formats are currently
                           ;; folded in with the geom modules.
                           ;;
                           (otherwise `(progn ,@body)))
                      (setq ,flag nil)))
           (when (and (or (stringp ,stream-or-file) (pathnamep ,stream-or-file)) (streamp *stream*))
             (close *stream* :abort ,flag))) nil))))


(defmacro write-env (&rest method-calls)
  "Void [Macro] (usually used just for outputting). Within the context of a <tt>with-format</tt>, calls functions of
the format object, optionally with arguments. Typically these functions will output data to the 
<tt>stream</tt> established by the <tt>with-format</tt>.

:arguments (function-calls \"(&rest). Functions on the named output-format to be called.\")

:example
<pre>
 (with-format (base-format my-object) (write-env (:a \"Hello World, my object's length is: \")
                                                 (:a (the length))))
</pre>"

  `(progn
     ,@(mapcar #'(lambda(method-call)
                   (typecase method-call
                     (string `(write-string ,method-call *stream*))
                     (otherwise `(,(glisp:intern (symbol-name (first method-call)) :gdl-format)
                                  *%format%* ,@(rest method-call)))))
               method-calls)))


(defmacro write-the (&rest args)
  "Lisp object [Macro]. Typcially used only to send output, not for the return value. This macro
is used within the body of a <tt>with-format</tt>. It sends the <tt>reference-chain</tt> to 
<tt>self</tt>, which typically means it is used within the context of a define-object where 
self is automatically lexically bound.

The reference-chain must terminate with an output-function defined for
the combination of the output-format specified in the enclosing
<tt>with-format</tt>, and the object identified by <tt>self</tt>. 

:arguments (reference-chain \"(&rest). A spliced-in list of symbols naming messages, which can 
be slots or objects starting from <tt>self</tt>, terminating with the name of an output-function. 
For referring to elements of a quantified set, or for passing arguments to GDL functions which 
take arguments, use parentheses around the message name and enclose the quantified element index 
or function arguments after the message name.\")"
  `(write-the-object self ,@args))


(defmacro write-the-object (object &rest reference-chain)
  "Lisp object [Macro]. Typcially used only to send output, not for the return value. This macro
is used within the body of a <tt>with-format</tt>. It sends the <tt>reference-chain</tt> to 
<tt>object</tt>, which must be specified as a Lisp expression (e.g. a variable) which 
evaluates to a GDL object.

The reference-chain must terminate with an output-function defined for the combination of 
the output-format specified in the enclosing <tt>with-format</tt>, and the object 
identified by <tt>object</tt>. 

:arguments (reference-chain \"(&rest). A spliced-in list of symbols naming messages, which can 
be slots or objects starting from <tt>object</tt>, terminating with the name of an output-function. 
For referring to elements of a quantified set, or for passing arguments to GDL functions which 
take arguments, use parentheses around the message name and enclose the quantified element index 
or function arguments after the message name.\")"

  (when (null reference-chain)
    (error "write-the-object must be given at least one item after the object"))
  (let* ((last (ensure-list (lastcar reference-chain)))
         (butlast (butlast reference-chain))
         (message (glisp:intern (symbol-name (first last)) :gdl-output))
         (args (rest last)))
    
    `(,message *%format%* (the-object ,object ,@butlast) *skin* ,@args)))


(defparameter *skin* t)

(defmacro format-slot (slot-name)
  "Lisp object [Macro]. Returns the value of the given slot within the context of the current 
<tt>with-format</tt> output format object.
:arguments (slot-name \"Symbol.\")"
  `(slot-value *%format%* ',(glisp:intern (symbol-name slot-name) :gdl-acc)))


(defmacro set-format-slot (slot-name value)
  "Void [Macro]. Sets the value of the given slot within the context of the current
<tt>with-format</tt> output format object.
:arguments (slot-name \"Symbol.\" value \"Lisp Value\")"  
  `(setf (slot-value *%format%* ',(glisp:intern (symbol-name slot-name) :gdl-acc)) ,value))


(defmacro with-format-slots ((&rest slots) &body body)
  "Void [Macro]. Wrap this around a body of code which should have access to multiple slots from the context
of the current <tt>with-format</tt> output format object.
:arguments (slots \"List of Symbols.\")"
  `(let (,@(mapcar #'(lambda(slot)
                       `(,slot (format-slot ,slot))) slots))
        ,@body))





