(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "load"))

(in-package :gdl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :gdl-surf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../smlib/gdl-smlib.asd"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :gdl-smlib))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-geometry-kernel :smlib :lib-path "../../common/staging/smlib.dll"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (glisp:set-gs-path "../../common/gpl/gs/gs8.63/bin/gswin32c.exe"))