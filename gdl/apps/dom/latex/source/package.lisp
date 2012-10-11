;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: user; Base: 10 -*-

(in-package :user)

(#+icad icad:def-icad-package 
	#-icad gdl:define-package :com.genworks.dom-latex
	(:use :com.genworks.dom :com.genworks.dom-writers))
