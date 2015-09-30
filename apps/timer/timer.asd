;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:timer :description
 "The Gendl™ timer Subsystem" :author b :license
 "Genworks Proprietary" :serial t :version "20150926" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/assembly") (:file "source/background-timer")
  (:file "source/initialize") (:file "source/publish")))
