;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: user; Base: 10 -*-

(in-package :user)

(#+icad icad:def-icad-package 
	#-icad gdl:define-package :com.genworks.dom-writers 
	(:use :com.genworks.dom)
	(:export plain-text latex html-write xml-write newline-count line-position))
								     
