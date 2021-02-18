;;;; utils.lisp

(in-package #:zacl)

(defparameter *aserve-files*
  '("htmlgen/htmlgen"
      "packages"
      "macs"
      "queue"
      "main"
      "headers"
      "parse"
      "decode"
      "publish"
      "authorize"
      "log"
      "client"
      "proxy"
      "cgi"
      "chunker")
  "Source files of aserve, taken from aserve/load.cl")

(defparameter *build-time-features*
  '(:smp :smp-macros :allegro))

(defun unused-lexical-warning-p (condition)
  (search "Unused lexical" (princ-to-string condition)))

(deftype unused-lexical-warning ()
  `(satisfies unused-lexical-warning-p))

(defun undefined-thing-warning-p (condition)
  (search ": Undefined" (princ-to-string condition)))

(deftype undefined-thing-warning ()
  `(satisfies undefined-thing-warning-p))

(defvar *undefined-things* (make-hash-table :test 'equalp))

(defun position-after-search (substring string)
  (let ((start (search substring string)))
    (when start
      (+ start (length substring)))))

(defun undefined-warning-thing (condition)
  (let* ((warning-text (princ-to-string condition))
         (pos (position-after-search ": Undefined " warning-text)))
    (when pos
      (subseq warning-text pos))))

(defun call-with-zacl-build-environment (fun)
  (let ((*readtable* zacl-reader:*allegro-rewriting-readtable*)
        (*package* (find-package :user))
        (*features* (append *build-time-features* *features*)))
    (handler-bind ((unused-lexical-warning #'muffle-warning)
                   (undefined-thing-warning
                    (lambda (c)
                      (incf (gethash (undefined-warning-thing c)
                                     *undefined-things*
                                     0)))))
      (funcall fun))))

(defmacro with-zacl-build-environment (&body body)
  `(call-with-zacl-build-environment (lambda () ,@body)))

(defun aload (file)
  (setf file (merge-pathnames file "file.cl"))
  (with-zacl-build-environment
    (load file)))

(defun acompile (file)
  (setf file (merge-pathnames file "file.cl"))
  (with-zacl-build-environment
    (compile-file file)))

(defun acl (file)
  (aload (acompile file)))


;;; Incremental retrying of stuff. Global state galore.

(defvar *to-build* *aserve-files*)
(defvar *source-directory* (merge-pathnames "src/aserve/"
                                            (user-homedir-pathname)))

(defun reset ()
  (clrhash *undefined-things*)
  (setf *to-build* *aserve-files*))

(defun undefined-report ()
  (let* ((alist (sort (hash-table-alist *undefined-things*) #'>
                     :key #'cdr))
         (total (length alist))
         (sum (reduce #'+ (mapcar 'cdr alist))))
    (format t "~D distinct undefined thing~:P~%" total)
    (format t "~D total undefined thing problem~:P~%" sum)
    (loop for (thing . count) in alist
          do (format t "  ~4D ~A~%"  count thing))))

(defun try (&optional harder)
  (unless *to-build*
    (unless harder
      (cerror "Call reset" "Nothing left to build -- (reset) to start over"))
    (reset))
  (let ((*default-pathname-defaults* *source-directory*))
    (loop
      (when (endp *to-build*)
        (return))
      (let ((file (first *to-build*)))
        (let ((fasl (acompile file)))
          (aload fasl)
          (pop *to-build*))))))
