;;;; -*- coding: utf-8 -*-
(in-package :asdf)

(defclass asdf::gdl (asdf::cl-source-file) ((type :initform "gdl")))
(defclass asdf::gendl (asdf::cl-source-file) ((type :initform "gendl")))
(defclass asdf::lisp (asdf::cl-source-file) ())


(defclass asdf::table (asdf::cl-source-file) ((type :initform "table")))

(defmethod perform ((o compile-op) (c asdf::table))
  (perform-peruse-file o c))

(defmethod perform ((o load-op) (c asdf::table))
    (perform-peruse-file o c))

(defun perform-peruse-file (o c)
  "Perform the perusal of icad-style catalog table."

  (declare (ignore o))
  
  (let ((input-file (slot-value c 'asdf/component:absolute-pathname)))

    (gendl::peruse-file input-file
			:load t)))
  
