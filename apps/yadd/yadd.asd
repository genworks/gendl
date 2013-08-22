;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:yadd :description
 "The Gendl™ Yet Another Definition Documenter (yadd)" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130822" :depends-on (:gwl-graphics)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/genworks")
  (:file "source/parameters") (:file "source/mixins")
  (:file "source/define-object-documentation")
  (:file "source/assembly") (:file "source/ass")
  (:file "source/format-documentation")
  (:file "source/function-documentation") (:file "source/initialize")
  (:file "source/publish") (:file "source/test-part")
  (:file "source/variable-documentation") (:file "source/zzinit")))
