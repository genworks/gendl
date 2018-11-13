;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:4bar :description
 "The Gendl® 4bar Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20181023" :depends-on (:bus)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components ((:gdl "source/4bar") (:gdl "source/bus")))
