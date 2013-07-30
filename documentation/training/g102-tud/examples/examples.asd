;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:examples :description
 "The Gendl™ examples Subsystem" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130710" :depends-on (:surf)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/ch2-examples") (:file "source/ch3-examples")
  (:gdl "source/primi-plane") (:gdl "source/ui-primi-plane")))

