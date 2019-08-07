;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gendl-asdf
  :description "asdf gendl artifacts loading"
  :serial t 
  :version "20161111" 
  :depends-on (#:gwl) ;; TODO reduce this one to a minimal dependency
  :components ((:file "gendl-asdf")))
