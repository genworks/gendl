;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:examples :description
 "The Gendl™ examples Subsystem" :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20130724" :depends-on (:surf)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/ch2-examples") (:file "source/ch3-examples")
  (:gdl "source/primi-plane") (:gdl "source/ui-primi-plane")))
