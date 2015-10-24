;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:timer :description
 "The Gendl™ timer Subsystem" :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20151024" :depends-on (:cl-smtp)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/assembly") (:file "source/background-timer")
  (:file "source/initialize") (:file "source/publish")))
