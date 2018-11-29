;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:geysr :description
 "The Gendl® geysr Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20181012" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:gdl "source/package") (:gdl "source/assembly")
  (:gdl "source/initialize") (:gdl "source/inspector")
  (:gdl "source/menu-node") (:gdl "source/menu") (:gdl "source/tree")
  (:gdl "source/user-inputs") (:gdl "source/viewport")))
