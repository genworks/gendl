;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:setup-cffi :description
 "The Gendl® setup-cffi Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20170828" :depends-on (:cffi)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components ((:file "source/setup")))
