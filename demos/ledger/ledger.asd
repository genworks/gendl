;;;; -*- coding: utf-8 -*-

(asdf:defsystem
 #:ledger
 :description
 "The Gendl™ Ledger Bookkeeping Demo"
 :author
 "Genworks International"
 :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial
 t
 :version
 "20140625"
 :depends-on
 (#-gwl :gwl)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package")
  (:file "source/assembly")
  (:file "source/html")
  (:file "source/zzinit")))
