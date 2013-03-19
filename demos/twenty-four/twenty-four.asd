
(asdf:defsystem #:twenty-four :description
 "The Gendl™ Arithmetic Card Game of Twenty-Four" :author
 "Genworks International and Gendl™ Project Contributors" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130319" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/infpre")
  (:file "source/cards-by-number") (:file "source/assembly")
  (:file "source/find")))
