;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:robot :description
 "The Gendl™ Simplified Android Robot example " :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20131206" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/initialize") (:file "source/parameters")
  (:file "source/zzinit")))
