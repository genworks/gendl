
(defsystem #:gdl-ta2
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
           (:gdl-gwl-graphics)
           :components
           ((:file "source/package") (:file "source/parameters")
            (:file "source/javascript")
            (:file "source/action-object") (:file "source/ajax")
            (:file "source/assembly") (:file "source/click-mode")
            (:file "source/inspector") (:file "source/object-tree")
            (:file "source/part-type-form") (:file "source/publish")
            (:file "source/viewport")))