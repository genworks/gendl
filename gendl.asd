(in-package :asdf)

(defsystem #:gendl
  :description "The Gendl Project"
  :author "Dave Cooper <david.cooper@genworks.com>"
  :license "Affero Gnu General Public License (please see http://www.gnu.org/licenses/)"
  :serial t
  #+asdf-utf-8 :encoding #+asdf-utf-8 :utf-8
  :version "2013031600"
  :depends-on (:cl-lite :gwl-graphics :tasty :robot :yadd)
  ;;
  ;; Leaving this out because we can't deal with swank package in the
  ;; monofasl which is to be loaded without swank. When/if we figure
  ;; out how to have swank already loaded into pre-built images, we
  ;; can have swank as one of the first dependencies, and put this
  ;; back.
  ;;
  ;;:components (#+:swank (:file "emacs/glime"))
  
  )



