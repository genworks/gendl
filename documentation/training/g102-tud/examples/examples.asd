;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:examples :description
 "The Gendl™ examples Subsystem" :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20130430" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:gdl "source/primi-plane") (:gdl "source/ui-primi-plane")))
