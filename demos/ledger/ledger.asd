;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:ledger :description
		"The Gendl™ Ledger Bookkeeping Demo" :author
		"Dave Cooper and Genworks International" :license
		"Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
		:serial t :version "20130408" :depends-on (:gwl)
		#+asdf-encoding :encoding #+asdf-encoding :utf-8
		:components
		((:file "source/package") (:file "source/assembly")
		 (:file "source/html")(:file "source/zzinit")))
