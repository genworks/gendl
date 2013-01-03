;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: training-g101; Base: 10 -*-

(in-package :training-g101)


(define-object assembly (slide-show-node)

  :input-slots
  ((title "G101: Common Lisp for GDL Developers")
   (slide-package (find-package :training-g101))
   (audio-base-url "/g101/mp3/")
   (image-base-url "/g101/images/")
   (style-url "/style/top.css"))

  
  :objects
  ((introduction    :type 'introduction)
   (welcome         :type 'welcome)
   (lists           :type 'lists)
   (control         :type 'control)
   (functions       :type 'functions)
   (input-output    :type 'input-output)
   (numbers         :type 'numbers)
   (data-structures :type 'data-structures)
   ;;(macros          :type 'macros)
   (symbols         :type 'symbols)
   (conclusion      :type 'conclusion)
   ))
