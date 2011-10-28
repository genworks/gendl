
(asdf:defsystem #:gdl-base :description
                "Auto-generated asdf defsys from Genworks GDL cl-lite."
                :author
                "Genworks and Dave Cooper unless otherwise indicated"
                :license "AGPL unless otherwise indicated" :serial t
                :version "2011102800" 
                :components
                ((:file "common/package")
		 (:file "common/genworks")
		 (:file "common/start-gdl")
		 (:file "prereqs/source/parameters")
                 (:file "prereqs/source/metaclasses")
                 (:file "prereqs/source/utilities")
                 (:file "expanders/source/inputs")
                 (:file "expanders/source/computed-slots")
                 (:file "expanders/source/functions")
                 (:file "expanders/source/methods")
                 (:file "expanders/source/objects")
                 (:file "macros/source/reference")
                 (:file "macros/source/message-utils")
                 (:file "macros/source/define-object")
                 (:file "macros/source/defaulting")
                 (:file "macros/source/define-format")
                 (:file "rest/source/parameters")
                 (:file "rest/source/aggregate")
                 (:file "rest/source/utilities")
                 (:file "rest/source/vanilla-mixin")
                 (:file "rest/source/base-rule-object")
                 (:file "rest/source/bodies")
                 (:file "rest/source/ignore-errors-with-backtrace")
                 (:file "rest/source/init")
                 (:file "rest/source/make-part")
                 (:file "rest/source/null-part")
                 (:file "rest/source/quantification")
                 (:file "rest/source/sequence")
                 (:file "tests/source/package")
                 (:file "tests/source/andrew")
                 (:file "tests/source/hello")
                 (:file "tests/source/minimal")))