(in-package :common-lisp-user)

(asdf:defsystem #:gendl
  :description "The Genworks GenDL Generative Programming and Knowledge Based Engineering System"
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "Affero Gnu General Public License (please see http://www.gnu.org/licenses/"
  :serial t
  :version "2013031000"
  :depends-on (:gwl-graphics
               :tasty
	       :robot
               :yadd))
               
               

               
