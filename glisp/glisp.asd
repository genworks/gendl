;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:glisp :description
 "The Gendl®  Common Lisp Portability" :author b :license
 "Genworks Proprietary" :serial t :version "20161223" :depends-on
 (:base :uiop :cl-typesetting :cl-ppcre :cl-who #-allegro :cl-base64 #-allegro :babel)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings #-(or allegro ccl sbcl) :acl-compat #+(or ccl sbcl) :zacl)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode (#-(or allegro ccl sbcl) :acl-compat #+(or ccl sbcl) :zacl)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/presets") (:file "source/parameters")
  (:file "source/genworks") (:file "source/initialize")
  (:file "source/zzinit")))
