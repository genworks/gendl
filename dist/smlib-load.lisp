(in-package :gdl-user)

(ql:quickload :gdl-surf)
(ql:quickload :uffi)
(load (merge-pathnames "smlib.fasl" *load-truename*))
(make-geometry-kernel :smlib :lib-path (merge-pathnames #+linux "smlib.so" #+mswindows "smlib.dll" *load-truename*))

