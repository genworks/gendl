
(asdf:defsystem #:base :description
                "Base GenDL Kernel Supplying define-object, define-format, define-view macros and Associated Referencing Operators and KBE Runtime Engine."
                :author
                "Dave Cooper and Genworks International"
                :license "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)" :serial t
                :version "2013031400"
                :components
                ((:file "common/package")
		 (:file "common/genworks")
		 (:file "common/start-gendl")
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
		 (:file "rest/source/quantification")
                 (:file "rest/source/aggregate")
                 (:file "rest/source/utilities")
                 (:file "rest/source/vanilla-mixin")
                 (:file "rest/source/base-rule-object")
                 (:file "rest/source/bodies")
                 (:file "rest/source/init")
                 (:file "rest/source/make-part")
                 (:file "rest/source/null-part")
                 (:file "rest/source/sequence")
		 (:file "rest/source/zzinit")))
