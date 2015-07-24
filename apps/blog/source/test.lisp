(ql:quickload :sqlite)

(defparameter *db-path* 
  (merge-pathnames "../db/test.db" (truename ".")))