
(asdf:defsystem #:tasty :description
 "The Gendl™ Web-based Development Environment (tasty)" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130324" :depends-on (:tree :gwl-graphics)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/assembly") (:file "source/initialize")
  (:file "source/inspector") (:file "source/menu")
  (:file "source/new-inspector") (:file "source/newer-inspector")
  (:file "source/part-type-form") (:file "source/publish")
  (:file "source/status-object") (:file "source/test-part")
  (:file "source/test-part2") (:file "source/two-viewports")
  (:file "source/viewport") (:file "source/zzinit")))
