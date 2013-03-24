
(asdf:defsystem #:wire-world :description
 "The Gendl™ Wire-World demo and test-case for wireframe tessellation and X3D output"
 :author "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130324" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components ((:file "source/package") (:file "source/assembly")))
