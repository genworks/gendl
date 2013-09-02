;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:4bar :description
 "The Gendl™ 4bar Subsystem" :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20130730" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components ((:gdl "source/4bar")))
