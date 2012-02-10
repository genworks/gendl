;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package :pdf)

(defmethod write-object ((obj indirect-object) &optional root-level)
  
  (gdl:print-variables *pdf-stream*)

  (if root-level
    (progn
      (vector-push-extend (format nil "~10,'0d ~5,'0d n "
                                  ;;FLAG -- DJC added conditional for allegro to handle aserve streams
                                  #+allegro (typecase *pdf-stream* 
					      ((excl::hiper-socket-stream
						net.aserve::chunking-stream)
					       (force-output *pdf-stream*) 
					       (excl::socket-bytes-written *pdf-stream*))
                                              (otherwise (file-position *pdf-stream*)))
                                  #+lispworks (if (typep *pdf-stream* 'acl-compat.socket::bidirectional-binary-socket-stream)
                                                  (progn (force-output *pdf-stream*)
                                                         (acl-compat.socket::stream-file-position *pdf-stream*))                                                  
                                                (file-position *pdf-stream*))
                                  #-(or allegro lispworks) (file-position *pdf-stream*)
                                  (gen-number obj))
                          *xrefs*)
      (format *pdf-stream* "~d ~d obj~%" (obj-number obj)(gen-number obj))
      (when (content obj)(write-object (content obj)))
      (write-string "endobj" *pdf-stream*)
      (write-char #\Newline *pdf-stream*))
    (format *pdf-stream* "~d ~d R" (obj-number obj)(gen-number obj))))


(defmethod write-document ((s stream) &optional (document *document*))
  
  (gdl:print-variables s)

  (let ((*xrefs* (make-array 10 :adjustable t :fill-pointer 0))
	startxref
	(*pdf-stream* s))
    ;;(with-standard-io-syntax
    #+lispworks (if (typep s 'acl-compat.socket::bidirectional-binary-socket-stream)
		    (setf acl-compat.socket::*socket-bytes-written* 0))

    (let ((*read-default-float-format* 'double-float))
      (process-outlines document)
      (vector-push-extend "0000000000 65535 f " *xrefs*)
      (write-line +pdf-header+ *pdf-stream*)
      (loop for obj across (objects document)
	 for first = t then nil
	 if obj do (write-object obj t)
	 else do (unless first (vector-push-extend "0000000000 00001 f " *xrefs*)))
       
      ;;(setf startxref (file-position s))
       
       
      (setf startxref #+allegro (typecase *pdf-stream* 
				  ((excl::hiper-socket-stream net.aserve::chunking-stream)
				   (force-output *pdf-stream*)
				   (excl::socket-bytes-written *pdf-stream*))
				  (otherwise (file-position *pdf-stream*)))
	     
	    #+lispworks (if (typep s 'acl-compat.socket::bidirectional-binary-socket-stream)
			    (progn (force-output s)
				   (acl-compat.socket::stream-file-position s))
			    (file-position s))
	    #-(or allegro lispworks) (file-position s))
       
      (unless startxref (setf startxref (file-position s)))
       

       
      (format *pdf-stream* "xref~%0 ~d~%" (length *xrefs*))
      (loop for xref across *xrefs*
	 do (write-line xref s))
      (format s "trailer~%<< /Size ~d~%/Root " (length *xrefs*)) ;(1- (length (objects document))))
      (write-object (catalog document))
      (when (docinfo document)
	(format s " /Info ")
	(write-object (docinfo document)))
      (format s "~%>>~%startxref~%~d~%%%EOF~%" startxref))))

