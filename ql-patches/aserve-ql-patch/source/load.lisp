(in-package :cl-user)

(when (com.genworks.lisp:featurep :allegro)
  (load
   (merge-pathnames
    "quicklisp-local/portableaserve-20110730-cvs/aserve/aserve.asd-dummy"
    com.genworks.lisp:*genworks-source-home*)))