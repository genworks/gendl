
(asdf:defsystem #:gdl-yadd :description
                "Auto-generated asdf defsys from Genworks GDL cl-lite."
                :author
                "Genworks and Dave Cooper unless otherwise indicated"
                :license "AGPL unless otherwise indicated" :serial t
                :version "2011102100" :depends-on (:gdl-gwl-graphics)
                :components
                ((:file "source/package") (:file "source/mixins")
                 (:file "source/assembly") (:file "source/ass")
                 (:file "source/define-object-documentation")
                 (:file "source/format-documentation")
                 (:file "source/function-documentation")
                 (:file "source/publish") (:file "source/test-part")
                 (:file "source/variable-documentation")))