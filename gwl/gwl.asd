;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gwl :description
 "The Gendl® Generative Web Language (GWL)" :author
 "Dave Cooper, Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20170802" :depends-on
 (:bordeaux-threads :drakma :glisp #+zacl :aserve #-zacl :paserve :yason)
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-unicode :defsystem-depends-on #+asdf-unicode ()
 #+asdf-encodings :encoding #+asdf-encodings :utf-8
 :components
 ((:file "source/package") (:file "source/with-all-servers")
  (:file "source/defparameters") (:file "source/base-html-sheet")
  (:file "source/html-format") (:file "source/macros")
  (:file "source/presets") (:file "source/base-html-utils")
  (:file "source/ignore-errors-with-backtrace")
  (:file "source/utilities") (:file "source/answer")
  (:file "source/accessories") (:file "source/gdl-remote")
  (:file "source/vanilla-remote") (:file "source/base64-utils")
  (:file "source/color-palette") (:file "source/crawler")
  (:file "source/log-utils") (:file "source/new-urls")
  (:file "source/publish") (:file "source/remote-object")
  (:file "source/security-check-failed")
  (:file "form-elements/source/grid-form-element")
  (:file "form-elements/source/macros")
  (:file "form-elements/source/primitives")
  (:file "ajax/source/parameters") (:file "ajax/source/ajax")
  (:file "ajax/source/base-ajax-sheet")
  (:file "ajax/source/skeleton-ui-element")
  (:file "gwl-session/source/parameters")
  (:file "gwl-session/source/cleanup")
  (:file "gwl-session/source/functions")
  (:file "gwl-session/source/session-control-auto-refresh")
  (:file "gwl-session/source/session-control-mixin")
  (:file "gwl-session/source/session-recovery")
  (:file "gwl-session/source/session-report")
  (:file "js-libs/jquery/source/package")
  (:file "js-libs/jquery/source/slider-form-control")
  (:file "zzinit/source/initialize") (:file "zzinit/source/zzinit")))
