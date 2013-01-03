;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g102; Base: 10 -*-

(in-package :training-g105)

(define-object assembly (slide-show-node)
  
  :input-slots
  ((title "G105: Distributed GDL Quickstart")
   (slide-package (find-package :training-g105))
   (image-base-url "slide-show-images/")
   (style-url "/slide-show-style/top.css"))

   
  
  :objects
  (
   (:session-01 :type 'session-01)
   (:session-02 :type 'session-02)
   
   ))



