;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:tree :description
 "The Gendl® Tree component used by Tasty and potentially as a UI component on its own"
 :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20181023" :depends-on (:gwl-graphics)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/ajax") (:file "source/assembly")
  (:file "source/newertree")))
