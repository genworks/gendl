
(asdf:defsystem #:gdl-tutorial
  :description
  "Auto-generated asdf defsys from Genworks GDL cl-lite."
  :author
  "Genworks and Dave Cooper unless otherwise indicated"
  :license
  "AGPL unless otherwise indicated"
  :serial
  t
  :version
  "2012121300"
  :depends-on
  (:gdl-dom)
  :components
  ((:file "source/package") (:file "source/parameters")
   (:file "source/introduction") (:file "source/installation")
   (:file "source/basic-operation") (:file "source/upgrade-notes")
   (:gdl "source/understanding-common-lisp")
   (:gdl "source/understanding-gendl")
   (:gdl "source/advanced-common-lisp")
   (:gdl "source/advanced-gendl") (:file "source/assembly")
   (:file "source/gendl-geometry")
   (:file "source/tasty-environment")))