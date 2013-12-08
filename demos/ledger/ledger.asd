;;;; -*- coding: utf-8 -*-

(asdf:defsystem
 #:ledger
 :description
 "The Gendl™ Ledger Bookkeeping Demo"
 :author
 "Genworks International"
 :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial
 t
 :version
 "20131207"
 :depends-on
 (#-gwl :gwl)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package")
  (:file "source/assembly")
  (:file "source/html")
  (:file "source/zzinit")))
