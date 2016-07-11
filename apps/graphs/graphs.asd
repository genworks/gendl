;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:graphs :description
 "The Gendl™ graphs Subsystem" :author "Dave Cooper" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20160110" :depends-on (:gwl-graphics)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/publish") (:file "source/ui")))
