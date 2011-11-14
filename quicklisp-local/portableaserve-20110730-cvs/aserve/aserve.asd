;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for ASERVE meant to replace
;;;; aserve-cmu.system, but could replace all other systems, too.
;;;; (hint, hint)

(defpackage #:aserve-system
  (:use #:cl #:asdf))
(in-package #:aserve-system)

(defclass acl-file (cl-source-file) ())
(defmethod asdf:source-file-type ((c acl-file) (s module)) "cl")

;;;; ignore warnings
;;;;
;;;; FIXME: should better fix warnings instead of ignoring them
;;;; FIXME: (perform legacy-cl-sourcefile) duplicates ASDF code

(defclass legacy-acl-source-file (acl-file)
    ()
  (:documentation
   "Common Lisp source code module with (non-style) warnings.
In contrast to CL-SOURCE-FILE, this class does not think that such warnings
indicate failure."))

(defmethod perform ((operation compile-op) (c legacy-acl-source-file))
  (let ((source-file (component-pathname c))
        (output-file (car (output-files operation c)))
        (warnings-p nil)
        (failure-p nil))
    (setf (asdf::component-property c 'last-compiled) nil)
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (setq warnings-p t)))
                   ;; _not_ (or error (and warning (not style-warning)))
                   (error (lambda (c)
                            (declare (ignore c))
                            (setq failure-p t))))
      (compile-file source-file
                    :output-file output-file))
    ;; rest of this method is as for CL-SOURCE-FILE
    (setf (asdf::component-property c 'last-compiled) (file-write-date output-file))
    (when warnings-p
      (case (asdf::operation-on-warnings operation)
        (:warn (warn "COMPILE-FILE warned while performing ~A on ~A"
                     c operation))
        (:error (error 'compile-warned :component c :operation operation))
        (:ignore nil)))
    (when failure-p
      (case (asdf::operation-on-failure operation)
        (:warn (warn "COMPILE-FILE failed while performing ~A on ~A"
                     c operation))
        (:error (error 'compile-failed :component c :operation operation))
        (:ignore nil)))))

#+(or lispworks cmu sbcl mcl openmcl clisp)
(defsystem aserve
    :name "AllegroServe (portable)"
    :author "John K. Foderaro"
    :version "1.2.35"
    :licence "LLGPL"
    :default-component-class acl-file
    :components ((:file "packages")
                 (:file "macs" :depends-on ("packages"))
                 (:legacy-acl-source-file "main" :depends-on ("macs"))
                 (:file "headers" :depends-on ("main"))
                 (:legacy-acl-source-file "parse" :depends-on ("main"))
                 (:file "decode" :depends-on ("main"))
                 (:file "publish" :depends-on ("main"))
                 (:file "authorize" :depends-on ("main" "publish"))
                 (:file "log" :depends-on ("main"))
                 (:file "client" :depends-on ("main"))
                 (:file "proxy" :depends-on ("main" "headers")))
    :depends-on (htmlgen acl-compat)
    :perform (load-op :after (op aserve)
                      (pushnew :aserve cl:*features*)))

#+allegro
(defclass original-aserve (asdf:component)
  ((loaded :initform nil :accessor loaded)))

;;
;; DJC
;;
#+allegro
(defmethod asdf:source-file-type ((c original-aserve) (s module)) "dummy")
;;
;; /DJC
;;


#+allegro
(defmethod asdf:perform ((op asdf:load-op) (c original-aserve))
  #+common-lisp-controller (c-l-c:original-require 'aserve)
  #-common-lisp-controller (require 'aserve)
  (setf (loaded c) t))

#+allegro
(defmethod asdf:operation-done-p ((op asdf:load-op) (c original-aserve))
  (loaded c))

#+allegro
(defmethod asdf:operation-done-p ((op asdf:compile-op) (c original-aserve))
  t)

#+allegro
(defsystem aserve
    :components ((:original-aserve "dummy")))

;;; Logical pathname is needed by AllegroServe examples
#+(or lispworks cmu mcl openmcl clisp sbcl)
(setf (logical-pathname-translations "ASERVE")
      `(
        #+ignore                        ; Don't need this with asdf
        ("**;*.lisp.*" ;,(logical-pathname "**;*.cl.*")
         ,(merge-pathnames 
           (make-pathname :host (pathname-host *load-truename*)
                         :directory '(:relative "aserve" 
                                                 :wild-inferiors)
                         :name :wild
                         :type "cl"
                         :version :wild)
           *load-truename*
         ))
        ("**;*.*.*" 
         ,(merge-pathnames 
           (make-pathname :host (pathname-host *load-truename*)
                          :directory '(:relative :wild-inferiors)
                          :name    :wild
                          :type    :wild
                          :version :wild
                          ;:case :common
                          )
           *load-truename*))))
#+cmu
(defun cl-user::init-aserve-cmu ()
  ;; this isn't strictly necessary, but scheduling feels very coarse
  ;; without startup-idle-and-top-level-loops, leading to answer delays
  ;; of about 1s per request.
  (unless (find-if
         #'(lambda (proc) (string= (mp:process-name proc) "Top Level Loop"))
         (mp:all-processes))
  (mp::startup-idle-and-top-level-loops)))


