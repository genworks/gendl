(in-package :common-lisp-user)

(let ((personal-boot-file "../gdl/tools/boot/load.lisp"))
  (when (probe-file personal-boot-file) (load personal-boot-file)))

(funcall (find-symbol (string '#:quickload) :ql) :gendl)

(gendl:start-gendl!)
