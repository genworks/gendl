;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g102; Base: 10 -*-

(in-package :training-g102)

(define-object assembly (slide-show-node)
  :input-slots
  ((title "G102: GDL Quickstart")
   (slide-package (find-package :training-g102))
   (image-base-url "/g102/images/")
   (style-url "/g102/style/top.css"))

   
  
  :objects
  (
   (:session-01 :type 'session-01)
   (:session-02 :type 'session-02)
   (:session-03 :type 'session-03)
   (:session-04 :type 'session-04)
   (:session-05 :type 'session-05)
   (:session-06 :type 'session-06)
   (:session-07 :type 'session-07)
   ))



