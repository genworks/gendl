
(in-package :gdl-lift-utils)

(defparameter *lift-data-directory* 
  (merge-pathnames (asdf:system-relative-pathname "regression" "data/")))
