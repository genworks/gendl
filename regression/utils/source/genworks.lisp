(in-package :com.genworks.lisp)


(defun close-old-areas ()
  #+allegro
  (setf (sys:gsgc-parameter :open-old-area-fence) -1)
  #-allegro (warn "Learn how to close old areas in currently running Lisp.~%"))


(defun open-old-areas ()
  #+allegro
  (setf (sys:gsgc-parameter :open-old-area-fence) 0)
  #-allegro (warn "Learn how to open old areas in currently running Lisp.~%"))

(defun get-mem-info ()
  #+allegro-v82 (excl.osi:get-mem-info)
  #-allegro (progn (warn "Learn how to get mem info in currently running Lisp.~%") (values 0 0)))

(defun gc-scavenge ()
  #+allegro (excl:gc)
  #-allegro (warn "Learn how to get mem info in currently running Lisp.~%"))
