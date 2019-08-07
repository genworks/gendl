;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gendl :description
 "The Gendl® gendl Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20181129" :depends-on
 (:gwl-graphics :tasty #+nil :geysr :robot :yadd :cl-lite)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings #+nil :gendl-asdf)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode (#+nil :gendl-asdf)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components nil)
