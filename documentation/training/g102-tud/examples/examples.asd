
(asdf:defsystem #:examples :description
 "The Gendl™ examples Subsystem" :author
 "Genworks International and Gendl™ Project Contributors" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130319" :depends-on nil
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/ch2-examples") (:file "source/ch2-solutions")
  (:file "source/ch3-examples") (:file "source/primi-plane-1")
  (:file "source/primi-plane-2") (:file "source/primi-plane-3")
  (:file "source/primi-plane-4") (:file "source/primi-plane-5")
  (:file "source/primi-plane-6") (:gdl "source/primi-plane")
  (:gdl "source/ui-primi-plane")))
