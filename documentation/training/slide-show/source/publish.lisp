(in-package :slide-show)

;;
;; FLAG -- change these to source-code relative pathnames
;;
#+nil
(publish-directory 
 :prefix "slide-show-images/"
 :destination (format nil "~a" (translate-logical-pathname "genworks:gwl-apps;slide-show;images;")))

#+nil
(publish-directory 
 :prefix "slide-show-style/"
 :destination (format nil "~a" (translate-logical-pathname "genworks:gwl-apps;slide-show;style;")))
