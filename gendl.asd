(in-package :common-lisp-user)

(asdf:defsystem #:gendl
  :description "The Gendl Project"
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "Affero Gnu General Public License (please see http://www.gnu.org/licenses/"
  :serial t
  :version "2013031400"
  :depends-on (:gwl-graphics
               :tasty
	       :robot
               :yadd)
  
  :components ((:file "source/try")))

               
               

               
