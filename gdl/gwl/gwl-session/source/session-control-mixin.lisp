;;
;; Copyright 2002-2011 Genworks International and Genworks BV 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 

(in-package :gwl)
;;--------------------------------------------------
;; Author : Brian Sorg, Liberating Insight
;; Revised: David Cooper, Genworks, 2004-09-16
;;
;; Date : created September 10
;;
;; Copyright September 2004 Liberating Insight LLC
;;-------------------------------------------------


(define-object session-control-mixin ()
  
  :documentation (:description "Mixin to the root object of the part which you wish to have session control over"
                  :author "Brian Sorg, Liberating Insight LLC (revised Dave Cooper, Genworks)")
  
  :input-slots
  (("Universal time after which the session should expire" 
    expires-at nil :settable)
   
   ("Length of time a session should last without activity in minutes"
    session-duration 20)

   ("Boolean. Determines whether expired sessions are replaced by recovery object. Default is nil."
    use-recovery-object? nil)
   
   ("Expiration time of the recovery object. After the recovery object has replaced the orginal 
instance at what time should the recovery instance expire?"
    recovery-expires-at (+ (get-universal-time) (* 60 (the session-duration))))
   
   ("Url to which a user will be redirected if requesting a session that has been cleared"
    recovery-url *recovery-url-default*)
   
   ("Type of original object, useful when viewing session report log"
    org-type (type-of self))
      
   (;; For future use
    %state-plist% '(0)))
  
  :functions
  (("This is the function called to check for and handle session control
:&key ((debug? nil) \"Boolean. Prints debug statement if needed\")" 
    clear-expired-session
    (&key debug?)
    (when debug?
     (format t "Validating instance ~a~% of class ~a~% Current Universal Time: ~a.~% Expiration Time: ~a.~% Clear now? ~a.~%"
                (the instance-id)
                (class-of self)
                (get-universal-time)
                (the expires-at)
                (the (clear-now?))))
    (when (the clear-now?)
      (the (session-clean-up))
      (glisp:w-o-interrupts (clear-instance (the instance-id))
        (when (and (the use-recovery-object?) (the recovery-expires-at))
          (let ((recovery (make-object 'session-recovery 
                                       :instance-id (the instance-id)
                                       :expires-at (the recovery-expires-at)
                                       :recovery-url (the recovery-url)
                                       :state-plist (the %state-plist%)
                                       :org-type (the org-type))))
            (setf (gethash (make-keyword (the instance-id)) *instance-hash-table*) 
                  ;;
                  ;; FLAG -- that last entry is actually supposed to be the skin class.
                  ;;
                  (list recovery (list 0) t)))))))
   ("Gets called right before the instance is going to get cleared. Is intended to be used to stop any instance states that may not be elequently handled by the garbage collector. ie database connections, multiprocessing locks, open streams etc."
    session-clean-up
    ())
      
   ("Boolean. Test to run to see if this session has expired and needs to be cleared now."
    clear-now?
    () (when (the expires-at) (> (get-universal-time) (the expires-at))))
   
   ("Method which will set the expires-at slot to the current time + the session-duration"
    set-expires-at
    (&optional (duration (the session-duration)))
    (the (set-slot! :expires-at (+ (get-universal-time) (* 60 duration)))))))




