;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:cl-lite :description
 "The Gendl™ Compile-and-Load Lite Utility" :author "John McCarthy"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20130430" :depends-on (:glisp)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/cl-lite") (:file "source/initialize")
  (:file "source/zzinit")))
