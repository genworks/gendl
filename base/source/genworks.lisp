;;;; -*- coding: utf-8 -*-
;;
;; Copyright 2002-2011, 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;

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

(in-package :com.genworks.lisp)
 
(defparameter *external-text-format*
    #+(and mswindows allegro) (excl:crlf-base-ef :1252)
    #-(and mswindows allegro) :default)

(defparameter *genworks-source-home* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun set-genworks-source-home-if-known ()
      (when (find-package :asdf)
	(multiple-value-bind (base-home error)
	    (ignore-errors (funcall (read-from-string "asdf:system-source-directory") "base"))
	  (if (typep error 'error)
	      (warn "~&ASDF is loaded, but :base is not registered. glisp:*genworks-source-home* remains unknown and set to nil.~%")
	      (setq *genworks-source-home*
		    (make-pathname :name nil
				   :type nil
				   :directory (butlast (butlast (pathname-directory base-home)))
				   :defaults base-home)))))))

(set-genworks-source-home-if-known)

(defparameter *gendl-source-home* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun set-gendl-source-home-if-known ()
      (when (find-package :asdf)
	(multiple-value-bind (gendl-home error)
	    (ignore-errors (funcall (read-from-string "asdf:system-source-directory") "gendl"))
	  (if (typep error 'error)
	      (warn "~&ASDF is loaded, but :base is not registered. glisp:*genworks-source-home* remains unknown and set to nil.~%")
	      (setq *gendl-source-home* gendl-home))))))

(set-gendl-source-home-if-known)


#-(or allegro lispworks sbcl ccl abcl ecl clisp) 
(error "Need implementation for command-line-arguments in currently running lisp.~%")
(defun basic-command-line-arguments ()
  #+allegro (sys:command-line-arguments :application nil)
  #+lispworks system:*line-arguments-list*
  #+sbcl sb-ext:*posix-argv*
  #+ccl (ccl::command-line-arguments)
  #+abcl extensions:*command-line-argument-list*
  #+ecl (loop for n from 0 below (si:argc) collect (si:argv n))
  #+clisp (coerce (ext:argv) 'list)
  )


#-(or allegro lispworks cmu sbcl ccl abcl ecl clisp) 
(error "Need implementation for executable-homedir-pathname for currently running lisp.~%")
(defun executable-homedir-pathname ()
  #+allegro (translate-logical-pathname "sys:")
  #+sbcl (make-pathname :name nil :type nil :defaults sb-ext:*core-pathname*)
  #+(or lispworks ccl ecl clisp) (make-pathname :name nil :type nil :defaults (first (glisp:basic-command-line-arguments)))
  #+abcl
  (warn "Don't know how to get executable-homedir-pathname on ~a! Please find out.~%"
	(lisp-implementation-type)))


(defparameter *gdl-program-home* #-abcl (glisp:executable-homedir-pathname)
	      #+abcl
	      (progn (warn "Don't know how to get executable-homedir-pathname on ~a! Please find out.~%"
			   (lisp-implementation-type)) nil))

(defparameter *gendl-program-home* *gdl-program-home*)


(defparameter *gdl-home* #-abcl 
  (make-pathname :name nil
 		 :type nil
		 :directory (butlast (pathname-directory *gdl-program-home*))
		 :defaults *gdl-program-home*)
  #+abcl
  (progn (warn "Don't know how to get *gdl-home* on ABCL! Please find out.~%")
	 nil))

(defparameter *gendl-home* *gdl-home*)


;;
;; FLAG -- bind these redefinitions more precisely/surgically.  for
;;         now we are using a sledgehammer and just turning off
;;         redefinition warnings until we get a better handle on
;;         things.
;;
#-(or allegro lispworks sbcl clisp) 
(warn "Need parameter for redefinition warnings for currently running lisp.~%")

(let (#+(or allegro lispworks clisp)
	(original-redefinition-warnings 
	 #+allegro excl:*redefinition-warnings*
	 #+lispworks lw:*redefinition-action*
	 #+clisp custom:*suppress-check-redefinition*))
  
  (defun begin-redefinitions-ok () 
    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    #+(or allegro lispworks clisp)
    (setq #+allegro excl:*redefinition-warnings* 
          #+lispworks lw:*redefinition-action* 
	  #+clisp custom:*suppress-check-redefinition* nil))
  (defun end-redefinitions-ok () 
    #+sbcl (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
    #+(or allegro lispworks clisp)
    (setq #+allegro excl:*redefinition-warnings* 
          #+lispworks lw:*redefinition-action* 
	  #+clisp custom:*suppress-check-redefinition*
	  original-redefinition-warnings)))


#-(or allegro lispworks cmu sbcl ccl abcl ecl clisp) 
(error "Need implementation for current-directory for currently running lisp.~%")
(defun current-directory ()
  #+allegro (excl:current-directory)
  #+lispworks (sys:current-directory)
  #+(or sbcl abcl ecl clisp)  *default-pathname-defaults*
  #+cmu (second (multiple-value-list (unix:unix-current-directory)))
  #+ccl (ccl:current-directory)
  
  )

;;
;; From SBCL manual (to avoid redef errors when compiling/loading defconstants in SBCL):
;;
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


#-(or allegro lispworks sbcl ccl abcl ecl clisp) 
(error "Need implementation for class-direct-superclasses for currently running lisp.~%")
(defun direct-superclasses (class)
  "Return a list of the direct superclasses."
  (#+(or allegro abcl) mop:class-direct-superclasses
   #+lispworks hcl:class-direct-superclasses 
   #+sbcl sb-mop:class-direct-superclasses 
   #+ccl ccl:class-direct-superclasses 
   #+(or ecl clisp) clos:class-direct-superclasses class))


(defun direct-superclass-names (class)
  "Return a list of names of the direct superclasses."
  (mapcar #'gl-class-name
          (direct-superclasses class)))


#-(or allegro lispworks cmu sbcl ccl abcl ecl clisp) 
(error "Need implementation for eql-specializer for currently running lisp.~%")
(defun eql-specializer (attr-sym)
  #+(or allegro abcl) (mop:intern-eql-specializer attr-sym)
  #+(or lispworks cmu) (list 'eql attr-sym)
  #+sbcl (sb-mop:intern-eql-specializer attr-sym)
  #+ccl (ccl:intern-eql-specializer attr-sym)
  #+(or ecl clisp) (clos:intern-eql-specializer attr-sym)
  )

(defun ensure-string (string-designator)
  (string string-designator))

(defun find-feature (feature) (find feature *features*))

#-(or allegro lispworks sbcl ccl abcl ecl clisp) 
(error "Need implementation of featurep for currently running lisp.~%")
;;
;; FLAG -- for full-featured featurep, use uiop:featurep
;;
(defun featurep (x) (find x *features*))

(defun find-feature-version (feature &optional (errorp t))
  (when (find-package :asdf)
    (let ((system (funcall (read-from-string "asdf:find-system")
			   (intern feature :keyword) errorp)))
      (when system (funcall (read-from-string "asdf:component-version") system)))))


(defun gl-class-name (class)
  "Return the class name."
  (#-cmu class-name #+cmu mop:class-name class))


#-(or allegro lispworks cmu sbcl ccl abcl ecl clisp) 
(error "Need implementation for method-specializers for currently running lisp.~%")
(defun gl-method-specializers (method)
  "Return a list of method specializers for the given method."
  (#+(or allegro abcl) mop:method-specializers  
   #+lispworks hcl:method-specializers
   #+cmu pcl:method-specializers 
   #+sbcl sb-mop:method-specializers 
   #+ccl ccl:method-specializers 
   #+(or ecl clisp) clos:method-specializers
   method))


(defun hex-string-to-integer (string)
  #+allegro(excl:hex-string-to-integer string)
  #-allegro 
  (let ((*read-base* 16) *read-eval*) (read-from-string string)))

;;
;; FLAG -- where ever this is used, we can pass it a raw symbol now -
;; no more need to do (symbol-name ...) or (string ...) around the argument
;; to this.
;;
(defun intern (symbol &optional (package *package*))
  (common-lisp:intern (string symbol) package))

#-allegro(warn "Find implementation for make-sans-value-equalp-hash-table for currently running lisp.~%")
(defun make-sans-value-equalp-hash-table ()
  "Make an equalp hash-table that acts like a set - just keys, no values."
  #+allegro (make-hash-table :values nil :test #'equalp)
  #-allegro (make-hash-table :test #'equalp))


#-allegro(warn "Find implementation for make-sans-value-hash-table for currently running lisp.~%")
(defun make-sans-value-hash-table (&key (size nil))
  "Make a hash-table that acts like a set - just keys, no values."
  (if size
      #+allegro (make-hash-table :values nil :size size)
      #-allegro (make-hash-table :size size)
    #+allegro (make-hash-table :values nil)
    #-allegro (make-hash-table)))


(defun make-versioned-features (feature)
  (mapcar #'(lambda(string)
	      (intern string :keyword))
	  (remove 
	   nil
	   (list feature (let ((version (find-feature-version feature nil)))
			   (when version (format nil "~a-~a" feature version)))))))

		    

#-(or allegro lispworks sbcl ccl abcl ecl clisp) 
(error "Need implementation for make-weak-hash-table for currently running lisp.~%")
#+ecl (warn "Need weak-hash-tables for ECL, or we will be running out of memory in web apps.~%")
(defun make-weak-hash-table (&rest args)
  (apply #'make-hash-table 
	 #+allegro :weak-keys #+allegro t
         #+allegro :values #+allegro :weak
         #+lispworks :weak-kind #+lispworks t
	 #+(or sbcl abcl) :weakness #+(or sbcl abcl) :key-and-value
	 #+clisp :weak #+clisp :key-and-value
	 #+ccl :weak #+ccl :key #+ccl :test #+ccl #'eq
         args))


#-(or allegro lispworks sbcl ccl clisp) 
(error "Need implementation for package-documentation for the currently running Lisp.~%")
(defun package-documentation (package)
  #+(or allegro lispworks ccl clisp) (documentation (find-package package) t)
  #+sbcl (sb-kernel:package-doc-string (find-package package)))


(defun set-default-character-width ()
  #+lispworks (lispworks:set-default-character-element-type 'lispworks:simple-char)

  )

(defun set-default-float-format ()
  #+allegro (tpl:setq-default *read-default-float-format* 'double-float)
  #+ccl (ccl::def-standard-initial-binding *read-default-float-format* 'double-float)
  (setq *read-default-float-format* 'double-float))


(defun set-default-package ()
  #+allegro (tpl:setq-default *package* (find-package :gdl-user))
  (setq *package* (find-package :gdl-user))
  #+allegro 
  (rplacd (assoc 'tpl::*saved-package*
                 tpl:*default-lisp-listener-bindings*) (find-package :gdl-user))
  #+allegro (top-level:do-command "package" "gdl-user"))

(defun set-defpackage-behavior ()
  #+lispworks (setq hcl:*handle-existing-defpackage* (list :add))
  #-lispworks nil ;; No action needed for non-lispworks platform currently.
  )


(defun set-settings (settings)
  (let (anything-changed?)
    (dolist (setting settings)
      (destructuring-bind (symbol default new-value-or-func) setting
	(let ((new-value (if (functionp new-value-or-func)
			     (funcall new-value-or-func) new-value-or-func)))
	  (unless (equalp (symbol-value symbol) new-value)
	    (setf (symbol-value symbol) new-value)
	    (setq anything-changed? t)
	    (format t "~&Set ~s from default value ~s to non-default value ~s.~%" 
		    symbol default new-value))))) anything-changed?))


(defun set-features (features)
  (let (anything-changed?)
    (dolist (feature features)
      (dolist (feature (make-versioned-features feature))
	(unless (or (null feature) (glisp:featurep feature))
	  (format t "~&Pushing ~s onto *features*.~%" feature)
	  (push feature *features*)
	  (setq anything-changed? t)))) anything-changed?))



#-allegro(warn "Find out how to retitle relevant windows in currently running lisp.~%")
#+(and allegro mswindows)
(defun set-window-titles ()
  (excl:console-control :title "Genworks Gendl® Console")
  ;;(retitle-emacs)
  )

#-(and allegro mswindows)
(defun set-window-titles ())

#-(or allegro lispworks abcl) (warn "Find out how to get the source-pathname  in current lisp.")
(defun source-pathname ()
  #+allegro excl:*source-pathname*
  #+lispworks dspec:*source-pathname*
  #+sbcl (error "need source-pathname in sbcl~%")
  #+ccl (error "need source-pathname in ccl~%")
  #+clisp (error "need source-pathname in ccl~%")
  #+abcl (extensions:source-pathname)
  )



#+nil
(defun retitle-emacs (&key (title "Genworks Gendl™ Interactive Authoring Environment"))
  "Retitles the associated GDL emacs window with the specified title.

:arguments (title \"The title to be placed on emacs title bar.\")"
  (declare (ignorable title))
  #+allegro 
  (when (lep:lep-is-running)
    (lep::eval-in-emacs "(defun frame-retitle (title)
                         (modify-frame-parameters  nil (list (cons 'name title))))")
    (lep::eval-in-emacs (concatenate 'string "(frame-retitle \"" title "\")"))))


(defun system-description (system-designator &optional (errorp t))
  (let (description (home (system-home system-designator errorp)))
    (when home 
      (let ((description-file (merge-pathnames "description.isc" home)))
	(when (probe-file description-file)
	  (setq description
		(with-open-file (in description-file :external-format #+lispworks :default #-lispworks :utf-8) (read in))))))
    (or description (format nil "~a Subsystem" system-designator))))


(defun system-home (system-designator &optional (errorp t))
  (if (find-package :asdf)
      (funcall (read-from-string "asdf:system-source-directory") system-designator)
      (when errorp (error "~&glisp:system-home was called, but cannot function because asdf is not loaded.~%"))))


(defun upcase (string)
  "Upcases the string, except in Allegro-CL's case-sensitive-lower mode, in which case the string is not modified."
  #-allegro (string-upcase string) 
  #+allegro (ecase excl:*current-case-mode* 
              (:case-insensitive-upper (string-upcase string))
              (:case-sensitive-lower string)))

(defmacro w-o-interrupts (&body body)
  (format t  "~&NOTE: w-o-interrupts is deprecated and ineffective in an SMP Lisp. 
glisp:w-o-interrupts becomes a simple progn currently.
This must be replaced with e.g. appropriate process-locks.~%")
  `(progn ,@body))


(defmacro without-package-variance-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
       ,@body)))


