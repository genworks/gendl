(in-package :common-lisp-user)


(asdf:defsystem #:genworks-gdl
  :description "The Genworks GenDL full system (synonym for :gdl-all)."
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "AGPL"
  :serial t
  :version "2012040100"
  :depends-on (:gdl-gwl-graphics
               :gdl-ta2
               :gdl-tasty
               :gdl-demos
               :gdl-yadd
               ;;:gdl-lift-utils
               ;;:gdl-geom-base-tests
               ;;:gdl-gwl-tests
               ;;:gdl-gwl-client-utilities
               ;;:gdl-gwl-graphics-tests
               ))
               
               

               
