;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:robot :description
 "The Gendl™ Simplified Android Robot example " :author
 "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20130501" :depends-on (:gwl-graphics)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/initialize") (:file "source/parameters")
  (:file "source/zzinit")))
