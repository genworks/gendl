;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:glisp :description
 "The Gendl®  Common Lisp Portability" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20170828" :depends-on
 ;;(:base :uiop :cl-typesetting :cl-ppcre :cl-who #-allegro :cl-base64 #-allegro :babel #-allegro :zacl)
 (:base :uiop :cl-typesetting :cl-ppcre :cl-who #-allegro :cl-base64 #-allegro :babel)
 :components
 ((:file "source/presets") (:file "source/parameters")
  (:file "source/genworks") (:file "source/initialize")
  (:file "source/zzinit")))
