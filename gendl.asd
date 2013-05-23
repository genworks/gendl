(in-package :asdf)

(defsystem #:gendl
  :description "The Gendl Project"
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "Affero Gnu General Public License (please see http://www.gnu.org/licenses/)"
  :serial t
  #+asdf-encoding :encoding #+asdf-encoding :utf-8
  :version "2013031600"
  :depends-on (:cl-lite :gwl-graphics :tasty :robot :yadd)
  :components ((:file #+:swank  "emacs/glime")))


(defmethod perform :after ((op load-op) (system (eql (find-system "gendl"))))
  (declare (ignore op))
  ;; Check whether GENDL has been configured yet.
  (multiple-value-bind (found ignore system-pathname)
      (locate-system system)
    (declare (ignore ignore))
    (if found
        (let ((config-file (merge-pathnames "configure.el" system-pathname)))
          (unless (probe-file config-file)
            ;; Looks like this is the first time around.
            (configure-gendl config-file)))
      (warn "ASDF cannot locate the GENDL which it just loaded. Further configuration not possible."))))

(defparameter *setup-gendl-template* "~
;;                        CONFIGURE.EL
;;
;; The purpose of this document is to configure (a) the lisp
;; invocation to be used for running GENDL and (b) the location of
;; your quicklisp installation.
;;
;; This file will be written automatically when the GENDL system is
;; loaded for the first time. You may edit it yourself but do take
;; care: the file is loaded by both EMACS and Common Lisp. We suggest
;; you don't add any other forms to the file.~@{~2%~s~}")

(defparameter *setup-emacs-template* "

*********************************************************************

To configure your EMACS to work with GENDL, add the following to your
.emacs file:

  (defun start-gendl () (interactive) (load ~s))
  ;; (global-set-key [f10] 'start-gendl)

")


(defun configure-gendl (config-file)
  (unless (probe-file config-file)
    (let ((lisp-invocation (funcall (find-symbol (string 'basic-command-line-arguments) :glisp)))
          (path-to-quicklisp-helper (ql-setup:qmerge "slime-helper.el")))
      ;; This is unfortunate. Clozure won't give you the heap name
      ;; unless you chase after it specifically.
      #+:ccl
      (let ((heap ccl:*heap-image-name*))
        (setf lisp-invocation `(,(car lisp-invocation) "-I" ,heap ,@(cdr lisp-invocation))))

      ;; 1. Write the config file.
      (with-open-file (out config-file :direction :output)
        (let ((*package* (find-package :common-lisp-user))
              (*print-case* :downcase))
          (format out *setup-gendl-template*
                  `(setq ,'cl-user::lisp-invocation ',lisp-invocation)
                  `(setq ,'cl-user::path-to-quicklisp-helper ,(namestring path-to-quicklisp-helper)))))

      ;; 2. Download quicklisp-slime-helper
      (let* (;; We intend to overwrite the default output from quicklisp-slime-helper:install
             (standard-output *standard-output*)
             (*standard-output* (make-broadcast-stream)))
        (handler-bind ((serious-condition (lambda (e) (declare (ignore e)) (setf *standard-output* standard-output))))
          (ql:quickload :quicklisp-slime-helper)))

      ;; 3. Add GENDL-loading form to .emacs
      (let ((path-to-gdl-loader (merge-pathnames "emacs/gdl.el" config-file)))
        (format t *setup-emacs-template* (namestring path-to-gdl-loader)))

      config-file)))

