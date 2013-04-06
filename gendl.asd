(in-package :common-lisp-user)

(asdf:defsystem #:gendl
  :description "The Gendl Project"
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "Affero Gnu General Public License (please see http://www.gnu.org/licenses/"
  :serial t
  #+asdf-encoding :encoding #+asdf-encoding :utf-8
  :version "2013031600"
  :depends-on (:cl-lite :gwl-graphics :tasty :robot :yadd))



               
