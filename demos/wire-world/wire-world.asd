;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:wire-world :description
 "The Gendl® Wire-World demo and test-case for wireframe tessellation and X3D output"
 :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20181023" :depends-on
 (#-gwl-graphics :gwl-graphics)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components ((:file "source/package") (:file "source/assembly")))
