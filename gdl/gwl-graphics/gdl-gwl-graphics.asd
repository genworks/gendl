
(defsystem #:gdl-gwl-graphics
           :description
           "Auto-generated asdf defsys from Genworks GDL cl-lite."
           :author
           "Genworks and Dave Cooper unless otherwise indicated"
           :license
           "AGPL unless otherwise indicated"
           :serial
           t
           :version
           "2013031000"
           :depends-on
           (:gdl-geom-base :gdl-gwl)
           :components
           ((:file "gwl/source/parameters")
            (:file "gwl/source/application-mixin")
            (:file "gwl/source/base-ajax-graphics-sheet")
            (:file "gwl/source/base-html-graphics-sheet")
            (:file "gwl/source/layout-mixin")
            (:file "gwl/source/process-graphics-fields")
            (:file "gwl/source/web-drawing")
            (:file "gwl/source/x3d-try")
            (:file "raphael/source/package")
            (:file "raphael/source/formats")
            (:file "raphael/source/lenses")))