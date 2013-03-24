
(asdf:defsystem #:glisp :description
 "The Gendl™  Common Lisp Portability" :author
 "Dave Cooper and Genworks International" :license
 "Gnu Affero General Public License (please see http://www.gnu.org/licenses/)"
 :serial t :version "20130324" :depends-on
 (:base :uiop :cl-typesetting :cl-ppcre :cl-who #-allegro :cl-base64 #-allegro :babel #-allegro :acl-compat)
 #+asdf-encoding :encoding #+asdf-encoding :utf-8
 :components
 ((:file "source/presets") (:file "source/genworks")
  (:file "source/initialize") (:file "source/parameters")
  (:file "source/zzinit")))
