;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gendl :description
 "The Gendl® gendl Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20180115" :depends-on
 (:bordeaux-threads :uiop :babel #-allegro :quri #-allegro :zacl :cl-lite :gwl-graphics :tasty :robot :yadd)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components ((:file "setup-cffi/source/setup")))
