
(asdf:defsystem #:translators :description
 "The Gendl™ Translators to/from XML and potentially other high-level KBE and Knowledge formats"
 :author "Genworks International and Gendl™ Project Contributors"
 :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130319" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "xml/source/package") (:file "xml/source/genworks")
  (:file "xml/source/definition-tree") (:file "xml/source/gdl2xml")
  (:file "xml/source/xml2gdl")))
