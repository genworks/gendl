;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:robot :description
 "The Gendl™ Simplified Android Robot example " :author
 "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20140227" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/initialize") (:file "source/parameters")
  (:file "source/zzinit")))
