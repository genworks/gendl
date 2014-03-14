;;;; -*- coding: utf-8 -*-

<<<<<<< Updated upstream
(asdf:defsystem #:robot :description
 "The Gendl™ Simplified Android Robot example " :author
 "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20140227" :depends-on nil
=======
(asdf:defsystem
 #:robot
 :description
 "The Gendl™ Simplified Android Robot example "
 :author
 "Genworks International"
 :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial
 t
 :version
 "20140313"
 :depends-on
 (:gwl-graphics)
>>>>>>> Stashed changes
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package")
  (:file "source/assembly")
  (:file "source/initialize")
  (:file "source/parameters")
  (:file "source/zzinit")))
