(in-package :timer-journaler)

(defparameter *debug?* nil)


(defparameter *source-path* 
  (make-pathname :directory (pathname-directory (glisp:source-pathname))
		 :name nil :type nil))

(defparameter *db-path* (merge-pathnames "../db/" *source-path*)
  "Pathname or string. Points to the directory containing the database files. 
This is expected to be set at startup in a runtime application.")


(defparameter *static-path* (merge-pathnames "../static/" *source-path*)
  "Pathname or string. Points to the directory containing the static files. 
This is expected to be set at startup in a runtime application.")


(defparameter *smtp-server* "localhost")

(setq gwl:*developing?* t)
(setq *debug?* t)
