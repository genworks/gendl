;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:glisp :description
 "The Gendl®  Common Lisp Portability" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20170524" :depends-on
 (:base :uiop :cl-typesetting :cl-ppcre :cl-who #-allegro :cl-base64 #-allegro :babel #-allegro :zacl)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings #-(or allegro ccl sbcl) :acl-compat
								      #+(or ccl sbcl) :zacl)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode (#-(or allegro ccl sbcl) :acl-compat
							#+(or ccl sbcl) :zacl)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/presets") (:file "source/parameters")
  (:file "source/genworks") (:file "source/initialize")
  (:file "source/zzinit")))
