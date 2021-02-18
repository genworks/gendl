;;;; package-ff.lisp

(in-package #:zacl)

(defvar *known-foreign-calls* (make-hash-table :test 'equal))

(defmacro define-known-foreign-call (name lambda-list &body body)
  `(progn
     (setf (gethash ',name *known-foreign-calls*)
           (list ',lambda-list ',body))))

(define-known-foreign-call "setuid" (id)
  (format t "; (setuid ~D) -- not implemented" id))

(define-known-foreign-call "setgid" (gid)
  (format t "; (setgid ~D) -- not implemented" gid))

(define-known-foreign-call "getpid" ()
  #+ccl
  (ccl::getpid))

(define-known-foreign-call "fork" ()
  (format t "; (fork) -- not implemented")
  (random 2))

(define-known-foreign-call "kill" (pid signal-number)
  (format t "; (kill ~D ~D) -- not implemented" pid signal-number))

(defun find-known-foreign-call (name)
  "If NAME is a known foreign call, return a list with its lambda-list and function body."
  (gethash name *known-foreign-calls*))



(defmacro ff:def-foreign-call ((lisp-name unix-name) &rest args)
  (declare (ignore args))
  (let ((fun-info (find-known-foreign-call unix-name)))
    (unless fun-info
      (error "Unknown foreign call -- ~S" unix-name))
    (destructuring-bind (lambda-list body)
        fun-info
      `(defun ,lisp-name ,lambda-list ,@body))))
