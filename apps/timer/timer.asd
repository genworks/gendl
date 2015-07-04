;;;; -*- coding: utf-8 -*-

(asdf:defsystem
 #:timer
 :description
 "The Gendl™ timer Subsystem"
 :author
 "Genworks International"
 :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial
 t
 :version
 "20150703"
 :depends-on
 nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package")
  (:file "source/assembly")
  (:file "source/background-timer")
  (:file "source/publish")))
