
(asdf:defsystem #:twenty-four :description
 "The Gendl™ Arithmetic Card Game of Twenty-Four" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130324" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/infpre")
  (:file "source/cards-by-number") (:file "source/assembly")
  (:file "source/find")))
