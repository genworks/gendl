;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:dom :description
 "The Gendl\" dom Subsystem" :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20160204" :depends-on (:cl-who :yadd)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "base/source/package") (:file "base/source/assembly")
  (:file "writers/source/package")
  (:file "writers/source/dom-writer") (:file "writers/source/html")
  (:file "writers/source/latex") (:file "writers/source/plain-text")
  (:file "html/source/package") (:file "html/source/lenses")
  (:file "latex/source/package") (:file "latex/source/lenses")
  (:file "latex/source/utilities")
  (:file "test-parts/source/assembly")))
