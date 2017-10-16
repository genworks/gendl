;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:yadd :description
 "The Gendl® Yet Another Definition Documenter (yadd)" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20170524" :depends-on
 (:gwl-graphics #-allegro :cl-html-parse)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode nil
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/genworks")
  (:file "source/parameters") (:file "source/mixins")
  (:file "source/define-object-documentation")
  (:file "source/assembly") (:file "source/ass")
  (:file "source/format-documentation")
  (:file "source/function-documentation") (:file "source/initialize")
  (:file "source/publish") (:file "source/test-part")
  (:file "source/variable-documentation") (:file "source/zzinit")))
