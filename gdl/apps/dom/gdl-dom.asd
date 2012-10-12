
(asdf:defsystem #:gdl-dom
  :description
  "Auto-generated asdf defsys from Genworks GDL cl-lite."
  :author
  "Genworks and Dave Cooper unless otherwise indicated"
  :license
  "AGPL unless otherwise indicated"
  :serial
  t
  :version
  "2012101200"
  :depends-on
  (:cl-who :gdl-yadd)
  :components
  ((:file "base/source/package")
   (:file "base/source/assembly")
   (:file "writers/source/package")
   (:file "writers/source/dom-writer")
   (:file "writers/source/html")
   (:file "writers/source/latex")
   (:file "writers/source/plain-text")
   (:file "html/source/package")
   (:file "html/source/lenses")
   (:file "latex/source/package")
   (:file "latex/source/lenses")
   (:file "latex/source/utilities")
   (:file "test-parts/source/assembly")))