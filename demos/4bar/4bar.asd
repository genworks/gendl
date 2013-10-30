;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:4bar :description
 "The Gendl™ 4bar Subsystem" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20131030" :depends-on (:bus)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components ((:gdl "source/4bar") (:gdl "source/bus")))
