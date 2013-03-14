(in-package :gwl)


(publish :path "/x3d-box-red"
	 :function #'(lambda(req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (let ((*stream* *html-stream*))
			     (with-cl-who ()
			       (:shape (:appearance ((:material :diffusecolor "1 0 0"))) ((:box :size "10 20 30")))))))))


(publish :path "/x3d-box-blue"
	 :function #'(lambda(req ent)
		       (with-http-response (req ent)
			 (with-http-body (req ent)
			   (let ((*stream* *html-stream*))
			     (with-cl-who ()
			       (:shape (:appearance ((:material :diffusecolor "0 0 1"))) ((:box :size "10 20 30")))))))))			     