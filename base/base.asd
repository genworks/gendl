;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:base :description
 "The Gendl™ Base Core Kernel Engine" :author "John McCarthy"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20131014" :depends-on nil
 #-asdf-unicode :defsystem-depends-on #-asdf-unicode (:asdf-encodings)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/genworks")
  (:file "source/presets") (:file "source/parameters")
  (:file "source/metaclasses") (:file "source/utilities")
  (:file "source/start") (:file "expanders/source/inputs")
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
  (:file "rest/source/ignore-errors-with-backtrace")
  (:file "rest/source/init") (:file "rest/source/make-part")
  (:file "rest/source/sequence") (:file "zzinit/source/initialize")
  (:file "zzinit/source/zzinit")))
