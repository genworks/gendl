(in-package :gdl-user)

(define-object time-report ()

  :computed-slots 
  ((update-flag nil :settable)


   (current-time-uncached (iso-8601-date (get-universal-time) :include-time? t) :uncached)
   
   (current-time (progn (the update-flag)
			(iso-8601-date (get-universal-time) :include-time? t)))))



#|
GDL-USER> (make-self 'time-report)
#<TIME-REPORT 4020123363>
GDL-USER> (the current-time)
"2015-06-12T16:34:20"
GDL-USER> (the current-time)
"2015-06-12T16:34:20"
GDL-USER> (the current-time)
"2015-06-12T16:34:20"
GDL-USER> (the (set-slot! :update-flag (not (the update-flag))))
NIL
GDL-USER> (the current-time)
"2015-06-12T16:34:41"
GDL-USER> (the current-time)
"2015-06-12T16:34:41"
GDL-USER> (the (set-slot! :update-flag (not (the update-flag))))
NIL
GDL-USER> (the current-time)
"2015-06-12T16:34:49"
GDL-USER> (the current-time)
"2015-06-12T16:34:49"
GDL-USER> (the current-time-uncached)
"2015-06-12T16:35:53"
GDL-USER> (the current-time-uncached)
"2015-06-12T16:35:53"
GDL-USER> (the current-time-uncached)
"2015-06-12T16:35:54"
GDL-USER> (the current-time-uncached)
"2015-06-12T16:35:55"
GDL-USER> (the current-time-uncached)
"2015-06-12T16:35:56"
GDL-USER> (the current-time-uncached)
"2015-06-12T16:35:57"
GDL-USER> 


|#
