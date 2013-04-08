;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:ledger :description
 "The Gendl™ Ledger Bookkeeping Demo" :author "John McCarthy"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20130406" :depends-on (:gwl)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/html")))
