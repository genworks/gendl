;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gorg :description
 "The Gendl® gorg Subsystem" :author "Xinyue (Cherie) Yang" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20170524" :depends-on (:html-template :gwl-graphics)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode nil
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/base-framework")
  (:file "source/publish")))
