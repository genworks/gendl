;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:base :description
 "The Gendl® Base Core Kernel Engine" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210202" :depends-on nil :defsystem-depends-on nil
 :components
 ((:file "source/package") (:file "source/genworks")
  (:file "source/presets") (:file "source/parameters")
  (:file "source/metaclasses") (:file "source/utilities")
  (:file "source/start") (:file "source/syntax-checker")
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
  (:file "rest/source/quantification")
  (:file "rest/source/aggregate") (:file "rest/source/utilities")
  (:file "rest/source/null-part") (:file "rest/source/vanilla-mixin")
  (:file "rest/source/base-rule-object") (:file "rest/source/bodies")
  (:file "rest/source/gdl-app")
  (:file "rest/source/ignore-errors-with-backtrace")
  (:file "rest/source/init") (:file "rest/source/make-part")
  (:file "rest/source/sequence") (:file "zzinit/source/initialize")
  (:file "zzinit/source/zzinit")))
