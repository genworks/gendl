(in-package :common-lisp-user)

(asdf:defsystem #:genworks-gdl
  :description "The Genworks GenDL full system (synonym for :gdl-all)."
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "AGPL"
  :serial t
  :version "2013031000"
  :depends-on (:gdl-gwl-graphics
               :gdl-tasty
	       :gdl-robot
               :gdl-yadd))
               
               

               
