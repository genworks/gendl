;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:examples :description
 "The Gendl™ examples Subsystem" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20140204" :depends-on (:gwl-graphics :surf)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/ch2-examples") (:file "source/ch3-examples")
  (:gdl "source/primi-plane") (:gdl "source/ui-primi-plane")))
