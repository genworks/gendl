(in-package :com.genworks.lisp)


(defun local-port (socket)
  (#+(or allegro zacl) socket:local-port
     #-(or allegro zacl abcl) acl-compat.socket:local-port
     #+abcl #'(lambda(socket) (declare (ignore socket) 0)) socket))

(defun remote-host (socket)
  #+(or allegro zacl) (socket:remote-host socket)
  #-(or allegro zacl abcl) (acl-compat.socket:remote-host socket))

;;
;; FLAG -- add abcl
;;
(defun local-host (socket)
  #+(or allegro zacl) (socket:local-host socket)
  #-(or allegro zacl abcl) (acl-compat.socket:local-host socket))


(defun with-timeout-sym ()
  "Returns the appropriate symbol for with-timeout, for substitution within macros."
  #+(or allegro zacl) 'sys:with-timeout
  #+abcl 'bordeaux-threads:with-timeout
  #-(or allegro zacl abcl) 'acl-compat.mp:with-timeout)

(defmacro with-timeout ((seconds &body timeout-body) &body body)
  #+(or allegro zacl) `(sys:with-timeout (,seconds ,@timeout-body) ,@body)
  #+abcl `(bordeaux-threads:with-timeout (,seconds ,@timeout-body) ,@body)
  #-(or allegro zacl abcl) `(acl-compat.mp:with-timeout (,seconds ,@timeout-body) ,@body))

