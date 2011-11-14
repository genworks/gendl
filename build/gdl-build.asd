
(asdf:defsystem #:gdl-build :description
                "Auto-generated asdf defsys from Genworks GDL cl-lite."
                :author
                "Genworks and Dave Cooper unless otherwise indicated"
                :license "AGPL unless otherwise indicated" :serial t
                :version "2011101800" :depends-on (:gdl-base)
                :components
                ((:file "source/package") (:file "source/genworks")
                 (:file "source/make-gdl-app")))