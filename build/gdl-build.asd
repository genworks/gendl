
(asdf:defsystem #:gdl-build :description
                "Auto-generated asdf defsys from Genworks GDL cl-lite."
                :author
                "Genworks and Dave Cooper unless otherwise indicated"
                :license "AGPL unless otherwise indicated" :serial t
                :version "2012090900" :depends-on (:gdl-geom-base)
                :components
                ((:lisp "source/package") (:lisp "source/genworks")
                 (:lisp "source/app") (:lisp "source/distro")))