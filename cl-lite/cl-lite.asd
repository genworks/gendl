
(asdf:defsystem #:cl-lite :description
 "The Gendl™ Compile-and-Load Lite Utility" :author
 "Genworks International and Gendl™ Project Contributors" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130319" :depends-on (:glisp)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/cl-lite") (:file "source/initialize")
  (:file "source/zzinit")))
