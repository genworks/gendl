
(in-package :gdl-lift-utils)

(defparameter *lift-data-directory* 
  (merge-pathnames "regression/data/" (or glisp:*genworks-source-home* glisp:*gdl-home*)))
