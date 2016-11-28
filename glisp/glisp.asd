;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:glisp :description
 "The Gendl®  Common Lisp Portability" :author b :license
 "Genworks Proprietary" :serial t :version "20161127" :depends-on
 (:base :uiop :cl-typesetting :cl-ppcre :cl-who #-allegro :cl-base64 #-allegro :babel #-(or allegro ccl) :acl-compat #+ccl :zacl)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/presets") (:file "source/parameters")
  (:file "source/genworks") (:file "source/initialize")
  (:file "source/zzinit")))
