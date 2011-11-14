(in-package :common-lisp-user)

(asdf:defsystem #:gdl-all
  :description "The Genworks GDL full system."
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "AGPL"
  :serial t
  :version "2011100200"
  :depends-on (
               :gdl-gwl-graphics
               :gdl-ta2
               :gdl-tasty
               :gdl-demos
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
               
               

               
