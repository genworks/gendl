
(asdf:defsystem #:gdl-build :description
                "Auto-generated asdf defsys from Genworks GDL cl-lite."
                :author
                "Genworks and Dave Cooper unless otherwise indicated"
                :license "AGPL unless otherwise indicated" :serial t
                :version "2012050300" :depends-on (:cl-fad)
                :components
                ((:file "source/package") (:file "source/genworks")
                 (:file "source/app") (:file "source/distro")))