(in-package :gwl)
;;--------------------------------------------------
;; Author : Brian Sorg, Liberating Insight
;; ;;
;; Date : created April 3rd, 2005
;;
;; Copyright April 2005 Liberating Insight LLC
;;
;; License:  Users of this file are granted the rights to distribute
;;           and use this software as governed by the terms of the
;;           Lisp Lesser GNU Public License
;;           (http://opensource.franz.com/preamble.html), also known
;;           as the LLGPL.
;;-------------------------------------------------


(defun session-report () 
  "Returns list of instances in a runtime environment. Those that are of type session-control-mixin, it provides more detailed information, that can be useful in tracking the session life. Currently, this is intended to run from the lisp command prompt."
  
  (let ((session-active 0)
	(session-recovery 0)
	(unknown 0))
  (maphash #'(lambda (k v) 
	       (let ((inst (car v)))
		 (if (typep inst 'session-control-mixin)
		     (let ((recovery? (typep inst 'session-recovery)))
		       (if recovery?
			   (incf session-recovery)
			 (incf session-active))
		       (format t "Key: ~a   Org Type:  ~s   Is Active?: ~a   Expires:   ~a~%"
			       k (the-object inst :org-type) (not (typep inst 'session-recovery))
			       (when (the-object inst expires-at)
				 (multiple-value-bind (sec min hr day mon yr)
				     (decode-universal-time (the-object inst expires-at))
				   (format nil "~d-~2,'0d-~2,'0d :: ~2,'0d:~2,'0d:~2,'0d"
					   yr mon day hr min sec)))))
		   (progn
		     (incf unknown)
		     (format t "~a ~s~%" k inst)))))
	   *instance-hash-table*)
  (format t "~2%************ Summary **************~%")
  (format t "
Active sessions:   ~d
Recovery sessions: ~d
Other sessions:    ~d
                  -----
Total sessions:    ~d"
	  session-active
	  session-recovery
	  unknown	  
	  (+ session-active session-recovery unknown))))
