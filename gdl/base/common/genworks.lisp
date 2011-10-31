;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
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

(in-package :gdl)

(eval-when (compile load eval)
  (defpackage :com.genworks.lisp 
    (:use :common-lisp)
    (:shadow #:intern)
    (:nicknames :glisp) 
    (:export #:*external-text-format*
             #:*genworks-source-home*
             #:basic-command-line-arguments
             #:begin-redefinitions-ok
             #:current-directory
             #:direct-superclasses
             #:direct-superclass-names
             #:end-redefinitions-ok
             #:eql-specializer
             #:get-backtrace
             #:gl-class-name
             #:gl-method-specializers
             #:hex-string-to-integer
             #:intern
             #:make-sans-value-equalp-hash-table
             #:make-sans-value-hash-table
             #:make-weak-hash-table
             #:set-default-float-format
             #:set-default-package
             #:set-defpackage-behavior
             #:set-local-compiler-tweaks
             #:set-window-titles
             #:upcase
             #:w-o-interrupts
             #:xref-off
             #:xref-on
             #:display-startup-banner
             )))


(in-package :com.genworks.lisp)

(defparameter *external-text-format*
    #+(and mswindows allegro) (excl:crlf-base-ef :1252)
    #-(and mswindows allegro) :default)

(defparameter *genworks-source-home* 
    (let ((gdl-base-home (asdf:system-source-directory "gdl-base")))
      (make-pathname :name nil
                     :type nil
                     :directory (butlast 
                                 (butlast (pathname-directory gdl-base-home)))
                     :defaults gdl-base-home)))


(defun basic-command-line-arguments ()
  #+allegro (sys:command-line-arguments :application nil)
  #+lispworks system:*line-arguments-list*
  #-(or allegro lispworks) 
  (error "Need implementation for command-line-arguments in currently running lisp.~%"))



(let ((original-redefinition-warnings 
       #+allegro excl:*redefinition-warnings*
       #+lispworks lw:*redefinition-action*
       #-(or allegro lispworks) 
       (error "Need parameter for redefinition warnings for currently running lisp.~%")))
  (defun begin-redefinitions-ok () 
    (setq #+allegro excl:*redefinition-warnings* 
          #+lispworks lw:*redefinition-action* nil))
  (defun end-redefinitions-ok () 
    (setq #+allegro excl:*redefinition-warnings* 
          #+lispworks lw:*redefinition-action* original-redefinition-warnings)))


(defun current-directory ()
  #+allegro (excl:current-directory)
  #+lispworks (sys:current-directory)
  #+cmu (second (multiple-value-list (unix:unix-current-directory)))
  #-(or allegro lispworks cmu) (error "Need implementation for current-directory for currently running lisp.~%"))


(defun direct-superclasses (class)
  "Return a list of the direct superclasses."
  (#+allegro mop:class-direct-superclasses
   #+lispworks hcl:class-direct-superclasses class)
  #-(or allegro lispworks) (error "Need implementation for class-direct-superclasses for currently running lisp.~%"))


(defun direct-superclass-names (class)
  "Return a list of names of the direct superclasses."
  (mapcar #-cmu #'class-name 
          #+cmu #'mop:class-name
          (direct-superclasses class)))


(defun eql-specializer (attr-sym)
  #+allegro (mop:intern-eql-specializer attr-sym)
  #+(or lispworks cmu) (list 'eql attr-sym))


;;
;; I believe this is from Hunchentoot:
;;
(defun get-backtrace (error)
  (with-output-to-string (s)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (*print-miser-width* 40)
            (*print-pretty* t)
            #+allegro(tpl:*zoom-print-circle* t)
            #+allegro(tpl:*zoom-print-level* nil)
            #+allegro(tpl:*zoom-print-length* nil))
        (ignore-errors
          (format *terminal-io* "~
~@<An unhandled error condition has been signalled:~3I ~a~I~:@>~%~%"
                  error))
        (ignore-errors
          (let ((*terminal-io* s)
                (*standard-output* s))
            #+allegro(tpl:do-command "zoom"
                            :from-read-eval-print-loop nil
                            :count t
                            :all t)))))))

(defun gl-class-name (class)
  "Return the class name."
  (#-cmu class-name #+cmu pcl:class-name class))

(defun gl-method-specializers (method)
  "Return a list of method specializers for the given method."
  (#+allegro mop:method-specializers 
   #+lispworks hcl:method-specializers
   #+cmu pcl:method-specializers method
   )
  #-(or allegro lispworks cmu) (error "Need implementation for class-direct-superclasses for currently running lisp.~%"))


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

(defun make-sans-value-equalp-hash-table ()
  "Make an equalp hash-table that acts like a set - just keys, no values."
  #+allegro (make-hash-table :values nil :test #'equalp)
  #-allegro (make-hash-table :test #'equalp))


(defun make-sans-value-hash-table (&key (size nil))
  "Make a hash-table that acts like a set - just keys, no values."
  (if size
      #+allegro (make-hash-table :values nil :size size)
      #-allegro (make-hash-table :size size)
    #+allegro (make-hash-table :values nil)
    #-allegro (make-hash-table)))


(defun make-weak-hash-table (&rest args)
  #-(or allegro lispworks) 
  (error "Need implementation for make-weak-hash-table for currently running lisp.~%")
  (apply #'make-hash-table #+allegro :weak-keys #+allegro t
         #+allegro :values #+allegro :weak
         #+lispworks :weak-kind #+lispworks t
         args))


(defun set-default-float-format ()
  #+allegro (tpl:setq-default *read-default-float-format* 'double-float)
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

(defun set-local-compiler-tweaks ()
  #+allegro 
  (when (boundp 'comp::*deprecate-peephole-and-save-arglist-switches*)
    (setf (symbol-value 'comp::*deprecate-peephole-and-save-arglist-switches*) t))
  #+allegro-cl-express (proclaim '(optimize (speed 1) (safety 1) (space 3)))
  #-allegro nil ;; No action needed for non-allegro platforms currently.
  )

(defun set-window-titles ()
  #+(and allegro mswindows)
  (excl:console-control :title "Genworks GDL Console")
  (retitle-emacs))


(defun retitle-emacs (&key (title "Genworks GDL Interactive Authoring Environment"))
  "Retitles the associated GDL emacs window with the specified title.

:arguments (title \"The title to be placed on emacs title bar.\")"
  (declare (ignorable title))
  #+allegro 
  (when (lep:lep-is-running)
    (lep::eval-in-emacs "(defun frame-retitle (title)
                         (modify-frame-parameters  nil (list (cons 'name title))))")
    (lep::eval-in-emacs (concatenate 'string "(frame-retitle \"" title "\")")))
  #-allegro
  (warn "NOTE -- we need to detect Emacs and possibly retitle Emacs window in currently running environment."))


(defun upcase (string)
  "Upcases the string, except in Allegro-CL's case-sensitive-lower mode, in which case the string is not modified."
  #-allegro (string-upcase string) 
  #+allegro (ecase excl:*current-case-mode* 
              (:case-insensitive-upper (string-upcase string))
              (:case-sensitive-lower string)))

(defmacro w-o-interrupts (&body body)
  (warn "Without-interrupts is deprecated in multiprocessing Lisp - replace usage with something else.")
  #-(or allegro lispworks cmu) (error "Need implementation for without-interrupts for currently running lisp.~%")
  `(#+allegro  excl:without-interrupts
    #+lispworks progn
    #+cmu system:without-interrupts 
    ,@body))


(defun xref-off (&optional include-source-info?)
  #-(or allegro lispworks) (error "Need implementation for xref-off for the currently running lisp.~%")
  (when include-source-info?
    (setq #+allegro excl:*load-source-file-info* 
          #+lispworks lw:*record-source-files*
          nil)
    (setq #+allegro excl:*record-source-file-info* 
          #+lispworks compiler:*source-file-recording*
          nil))
  (setq #+allegro excl:*load-xref-info* 
        #+lispworks compiler:*load-xref-info*
        nil)
  (setq #+allegro excl:*record-xref-info* 
        #+lispworks compiler:*produce-xref-info*
        nil))

  
(defun xref-on (&optional include-source-info?)
  #-(or allegro lispworks) (error "Need implementation for xref-off for the currently running lisp.~%")
  (when include-source-info?
    (setq #+allegro excl:*load-source-file-info* 
          #+lispworks lw:*record-source-files*
          t)
    (setq #+allegro excl:*record-source-file-info* 
          #+lispworks compiler:*source-file-recording*
          t))
  (setq #+allegro excl:*load-xref-info* 
        #+lispworks compiler:*load-xref-info*
        t)
  (setq #+allegro excl:*record-xref-info* 
        #+lispworks compiler:*produce-xref-info*
        t))

(defun display-startup-banner (edition banner)
  (ecase edition
    (:open-source (format t banner))))
     
