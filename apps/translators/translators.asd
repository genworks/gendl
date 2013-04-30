;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:translators :description
 "The Gendl™ Translators to/from XML and potentially other high-level KBE and Knowledge formats"
 :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20130428" :depends-on (:gwl)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "xml/source/package") (:file "xml/source/genworks")
  (:file "xml/source/definition-tree") (:file "xml/source/gdl2xml")
  (:file "xml/source/xml2gdl")))
