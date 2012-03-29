;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g102; Base: 10 -*-

(in-package :training-g108)

(define-object assembly (slide-show-node)
  :input-slots
  ((title "G108: Web/Ajax User Interface")
   (slide-package (find-package :training-g108))
   (image-base-url "/g108/images/")
   (style-url "/static/gwl/style/top.css"))

   
  
  :objects
  (
   (session-01 :type 'basic-sheet)
   (session-02 :type 'ajax-sections)
   

   ))





