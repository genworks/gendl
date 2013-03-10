(in-package :gdl-user)

#|

To use:

 1. (setq *compile-for-dgdl?* t)
 2. Compile & load this file in the "primary" Gendl session.
 3. Start a separate Gendl session (this should start by default on 9001)
 4. compile & load this file in the "secondary" Gendl session 
     *compile-for-dgdl?* should not be necessary on the secondary session but 
     will not hurt. 
 5. On the primary session, try:

    (make-self 'container)
    (the remote-1 slot-1)

   This should return "one"


|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *compile-for-dgdl?* t))

(define-object container ()
  :objects ((remote-1 :type 'remote-object
		      :remote-type 'remote-1
		      :port 9001
		      :host "localhost")))


(define-object remote-1 ()
  :computed-slots 
  ((slot-1 "one")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *compile-for-dgdl?* nil))