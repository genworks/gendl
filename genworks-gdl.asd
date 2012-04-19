(in-package :common-lisp-user)



(asdf:defsystem #:genworks-gdl
  :description "The Genworks GDL full system (synonym for :gdl-all)."
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "AGPL"
  :serial t
  :version "2012040100"
  :depends-on (:gdl-gwl-graphics
               :gdl-ta2
               :gdl-tasty
               ;;:gdl-demos
               :gdl-yadd
               ;;:gdl-geom-base  FLAG -- remove, this is depended on already by gdl-gwl-graphics.
               ;;:gdl-gwl  FLAG -- remove, this is depended on already by gdl-gwl-graphics.
               ;;:gdl-lift-utils
               ;;:gdl-geom-base-tests
               ;;:gdl-gwl-tests
               ;;:gdl-gwl-client-utilities
               ;;:gdl-gwl-graphics
               ;;:gdl-gwl-graphics-tests
               ;;:gdl-surf
               ;;:gdl-surf-tests
               ))
               
               

               
