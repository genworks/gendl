
(asdf:defsystem #:tree :description
 "The Gendl™ Tree component used by Tasty and potentially as a UI component on its own"
 :author "Genworks International and Gendl™ Project Contributors"
 :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130319" :depends-on (:gwl-graphics)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/ajax") (:file "source/assembly")
  (:file "source/newertree")))
