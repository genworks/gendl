
(asdf:defsystem #:wire-world :description
 "The Gendl™ Wire-World demo and test-case for wireframe tessellation and X3D output"
 :author "Genworks International and Gendl™ Project Contributors"
 :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130319" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components ((:file "source/package") (:file "source/assembly")))
