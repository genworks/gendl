;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:geysr :description
 "The Gendl® geysr Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20181204" :depends-on (:gwl-graphics)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings :gendl-asdf)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode (:gendl-asdf)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:gdl "source/package") (:gdl "source/parameters")
  (:gdl "source/assembly") (:gdl "source/initialize")
  (:gdl "source/inspector") (:gdl "source/menu-node")
  (:gdl "source/menu") (:gdl "source/tree") (:gdl "source/viewport")
  (:gdl "source/zzinit")))
