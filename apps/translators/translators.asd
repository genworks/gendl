;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:translators :description
 "The Gendl® Translators to/from XML and potentially other high-level KBE and Knowledge formats"
 :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20181023" :depends-on (:gwl)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "xml/source/package") (:file "xml/source/genworks")
  (:file "xml/source/definition-tree") (:file "xml/source/gdl2xml")
  (:file "xml/source/xml2gdl")))
