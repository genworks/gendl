;;;; -*- encoding: utf-8; -*-

(asdf:defsystem #:examples :description
 "The Gendl™ examples Subsystem" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130408" :depends-on (:gwl-graphics :surf)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:gdl "source/primi-plane") (:gdl "source/ui-primi-plane")))
