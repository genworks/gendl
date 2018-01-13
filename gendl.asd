;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gendl :description
 "The Gendl® gendl Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20170524" :depends-on
 (:bordeaux-threads :uiop :babel :quri :zacl :cl-lite :gwl-graphics :tasty :robot :yadd)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode nil
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components nil)
