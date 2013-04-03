
(asdf:defsystem #:wire-world :description
 "The Gendl™ Wire-World demo and test-case for wireframe tessellation and X3D output"
 :author "John McCarthy" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20130402" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components ((:file "source/package") (:file "source/assembly")))
