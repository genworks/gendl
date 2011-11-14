;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package :pdf)


(defmacro with-page ((&rest args) &body body)
  `(let* ((*page* (make-instance 'page ,@args)))
     (with-standard-io-syntax
       ;;
       ;; FLAG -- DJC set *read-default-float-format* to be compatible with GDL's default.
       ;;
       (let ((*read-default-float-format* 'double-float))
         (setf (content (content-stream *page*))
           (with-output-to-string (*page-stream*)
             ,@body))))
     t))

