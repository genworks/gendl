
(asdf:defsystem #:gdl-gwl :description
                "Auto-generated asdf defsys from Genworks GDL cl-lite."
                :author
                "Genworks and Dave Cooper unless otherwise indicated"
                :license "AGPL unless otherwise indicated" :serial t
                :version "2011101700" :depends-on
                (:gdl-base :gdl-cl-lite :aserve :cl-who :cl-ppcre
                 :cl-base64 :trivial-backtrace)
                :components
                ((:file "source/package") (:file "source/genworks")
                 (:file "source/defparameters")
                 (:file "source/base-html-sheet")
                 (:file "source/base-html-utils")
                 (:file "source/macros") (:file "source/ignore-errors-with-backtrace")
		 (:file "source/initialize")
                 (:file "source/utilities") (:file "source/answer")
                 (:file "source/accessories")
                 (:file "source/gdl-remote")
                 (:file "source/vanilla-remote")
                 (:file "source/base64-utils")
                 (:file "source/color-palette")
                 (:file "source/crawler") (:file "source/log-utils")
                 (:file "source/new-urls") (:file "source/publish")
                 (:file "source/remote-object")
                 (:file "source/security-check-failed")
                 (:file "ajax/source/parameters")
                 (:file "ajax/source/ajax")
                 (:file "ajax/source/base-ajax-sheet")
                 (:file "ajax/source/skeleton-ui-element")
                 (:file "form-elements/source/grid-form-element")
                 (:file "form-elements/source/macros")
                 (:file "form-elements/source/primitives")
                 (:file "form-elements/source/short-test")
                 (:file "form-elements/source/test-seq")
                 (:file "form-elements/source/validation-tests")
                 (:file "gwl-session/source/parameters")
                 (:file "gwl-session/source/cleanup")
                 (:file "gwl-session/source/functions")
                 (:file
                  "gwl-session/source/session-control-auto-refresh")
                 (:file "gwl-session/source/session-control-mixin")
                 (:file "gwl-session/source/session-recovery")
                 (:file "gwl-session/source/session-report")
                 (:file "js-libs/jquery/source/package")
                 (:file "js-libs/jquery/source/slider-form-control")))