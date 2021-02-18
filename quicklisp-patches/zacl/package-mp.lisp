;;;; package-mp.lisp
;;;
;;; Multiprocessing.
;;;

(in-package #:zacl)

(defmacro mp:with-process-lock ((lock-form) &body body)
  (declare (ignorable lock-form))
  `(with-lock-held (,lock-form) ,@body))

(defun mp:make-process-lock (&key name)
  (make-lock name))

(defmacro mp:without-scheduling (&body body)
  `(progn ,@body))


;;; Gates

(defgeneric mp:make-gate (initial-state))

(defgeneric mp:open-gate (gate))

(defgeneric mp:close-gate (gate))

(defgeneric mp:gate-open-p (gate))


;;; Processes

(defclass mp:process-lock (excl:lockable-object) ())

(defgeneric mp:make-process (&key name initial-bindings)
  (:method (&key name initial-bindings)
    (make-process name :class 'process
                  :initial-bindings initial-bindings)))

(defgeneric mp:process-thread (process))

(defgeneric mp:process-name (process)
  (:method (process)
    (process-name process)))

(def-fake-slot mp:process-keeps-lisp-alive-p process :default-value nil)

(defgeneric mp:process-add-run-reason (process object)
  (:method (process object)
    (add-run-reason process object)))

(defgeneric mp:process-revoke-run-reason (process object)
  (:method (process object)
    (revoke-run-reason process object)))

(defgeneric mp:process-allow-schedule ()
  (:method ()
    (yield (current-process))))

(defgeneric mp:process-kill (process)
  (:method (process)
    (kill process)))

(defgeneric mp:process-preset (process fun &rest args)
  (:method (process fun &rest args)
    (apply #'preset process fun args)))

(defgeneric mp:process-run-function (name-or-plist function &rest arguments)
  (:method ((name string) function &rest arguments)
    (apply #'mp:process-run-function (list :name name) function arguments))
  (:method ((plist list) function &rest arguments)
    (apply #'process-run-function plist
           function arguments)))

(defgeneric mp:process-run-reasons (process)
  (:method (process)
    (run-reasons process)))


;;; Queues

;; FIXME: Timeouts.

(defclass mp:queue ()
  ((queue
    :initarg :queue
    :accessor queue
    :initform (make-queue :simple-queue))
   (lock
    :initform (make-lock)
    :reader lock)))

(defgeneric mp:enqueue (queue thing)
  (:method (queue thing)
    (with-lock-held ((lock queue))
      (qpush (queue queue) thing))))

(defgeneric mp:dequeue (queue &key wait empty-queue-result whostate)
  (:method (queue &key wait empty-queue-result whostate)
    (declare (ignore whostate wait))
    (with-lock-held ((lock queue))
      (qpop (queue queue) empty-queue-result))))

(defun mp:wait-for-input-available (stream-or-fds
                                    &key wait-function whostate timeout)
  (declare (ignore stream-or-fds wait-function whostate timeout))
  ;; Only used by aserve cgi
  (error "Not implemented -- MP:WAIT-FOR-INPUT-AVAILABLE"))
