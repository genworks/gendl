;;;; -*- coding: utf-8 -*-
(in-package :gdl-user)

(defclass asdf::gdl (asdf::cl-source-file) ((type :initform "gdl")))
(defclass asdf::gendl (asdf::cl-source-file) ((type :initform "gendl")))
(defclass asdf::lisp (asdf::cl-source-file) ())


