(in-package :aserve-log)

(with-all-servers (server)
  (publish :path "/iiug-log"
	   :server server
	   :function #'(lambda(req ent)
			 (gwl-make-part req ent "aserve-log:assembly"  
					:make-object-args (list :relevant-hosts (list "www.iiug.com")))))
  (publish :path "/server-log"
	   :server server
	   :function #'(lambda(req ent)
			 (gwl-make-part req ent "aserve-log:assembly"))))
