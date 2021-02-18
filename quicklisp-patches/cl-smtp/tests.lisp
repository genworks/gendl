;; -*- mode: common-lisp; coding: utf-8 -*-
(in-package :cl-smtp)

(defparameter *cl-smtp-tests* (make-hash-table :test 'equal))

(defmacro define-cl-smtp-test (name (&rest args) &body body)
  (let ((tmpname (gensym (string name))))
    `(progn
       (defun ,tmpname (,@(mapcar #'car args))
         ,@body)
       (setf (gethash ,(string-downcase name) *cl-smtp-tests*)
             (list #',tmpname ,args)))))

(defun get-component-pathname ()
  (asdf:component-pathname (asdf:find-system "cl-smtp")))



(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-1" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :utf-8))))
    (assert qstr)
    (assert (string-equal qstr "=C3=B6=C3=BC=C3=A4=C3=96=C3=9C=C3=84=C3=9F"))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-2" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1))))
    (assert qstr)
    (assert (string-equal qstr "=F6=FC=E4=D6=DC=C4=DF"))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-3" ()
  (let* ((str "check if #\= encoded")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1))))
    (assert qstr)
    (assert (string-equal qstr "check if #\=3D encoded"))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-4" ()
  (let* ((str "Müde vom Durchwandern öder Letternwüsten, voll leerer Hirngeburten, in anmaaßendsten Wortnebeln ; überdrüssig ästhetischer Süßler wie grammatischer Wässerer ; entschloß ich mich : Alles, was je schrieb, in Liebe und Haß, als immerfort mitlebend zu behandeln !")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1 :columns 64))))
    (assert qstr)
    (assert (string-equal qstr "M=FCde vom Durchwandern =F6der Letternw=FCsten, voll leerer Hirngeburt=
en, in anmaa=DFendsten Wortnebeln ; =FCberdr=FCssig =E4sthetischer S=FC=DFle=
r wie grammatischer W=E4sserer ; entschlo=DF ich mich : Alles, was j=
e schrieb, in Liebe und Ha=DF, als immerfort mitlebend zu behandel=
n !"
))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-5" ()
  (let* ((str (format nil "check of masked dots.~C.~C.end" #\Newline #\Newline))
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream 
                  str s :external-format :latin-1 :columns 64))))
    (assert qstr)
    (assert (string-equal qstr (format nil "check of masked dots.~C~C..~C~C..end" #\Return #\Newline #\Return #\Newline)))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-6" ()
  (let* ((str (format nil "check of masked dot at last column 12345678901234567890123456789.~A~ALAST LINE" #\Newline #\Newline))
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream
                  str s :external-format :latin-1 :columns 64))))
    (assert qstr)
    (assert (string-equal qstr (format nil "check of masked dot at last column 12345678901234567890123456789=~C~C..~C~C~C~CLAST LINE" #\Return #\Newline #\Return #\Newline #\Return #\Newline)))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-7" ()
  (let* ((str (format nil "Example using non-ASCII characters:~A   -  74% : Erreur de protocole : la passerelle ne répond pas à la requête.~AYou won’t receive this!" #\Newline #\Newline))
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream
                  str s :external-format :utf-8 :columns 74))))
    (assert qstr)
    (assert (string-equal qstr (format nil "Example using non-ASCII characters:~C~C   -  74% : Erreur de protocole : la passerelle ne r=C3=A9pond pas =C3=A0 la requ=C3=AAte=~C~C..~C~CYou won=E2=80=99t receive this!" #\Return #\Newline #\Return #\Newline #\Return #\Newline)))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-8" ()
  (let* ((str "12345678901234567890123456789012345678901234567890123456789012345678901234.")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream
                  str s :external-format :utf-8 :columns 74))))
    (assert qstr)
    (assert (string-equal qstr (format nil"12345678901234567890123456789012345678901234567890123456789012345678901234=~C~C.." #\Return #\Newline)))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-9" ()
  (let* ((str
          "123456789012345678901234567890123456789012345678901234567890123456789012345.")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream
                  str s :external-format :utf-8 :columns 74))))
    (assert qstr)
    (assert (string-equal qstr (format
                                nil"12345678901234567890123456789012345678901234567890123456789012345678901234=~C~C5."
                                #\Return #\Newline)))))

(define-cl-smtp-test "rfc2045-q-encode-string-to-stream-10" ()
  (let* ((str "لینک تایید | Verification link")
         (qstr (with-output-to-string (s)
                 (rfc2045-q-encode-string-to-stream
                  str s :external-format :utf-8 :columns 74))))
    (assert qstr)
    (assert (string-equal qstr "=D9=84=DB=8C=D9=86=DA=A9 =D8=AA=D8=A7=DB=8C=DB=8C=D8=AF | Verification link"))))

(define-cl-smtp-test "string-has-non-ascii-1" ()
  (assert (string-has-non-ascii "test Ü ende")))

(define-cl-smtp-test "string-has-non-ascii-2" ()
  (assert (not (string-has-non-ascii "test ende"))))

(define-cl-smtp-test "rfc2045-q-encode-string-utf-8-1" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (rfc2045-q-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal 
             qstr "=?UTF-8?Q?=C3=B6=C3=BC=C3=A4=C3=96=C3=9C=C3=84=C3=9F?="))))

(define-cl-smtp-test "rfc2045-q-encode-string-utf-8-2" ()
  (let* ((str "لینک تایید | Verification link")
         (qstr (rfc2045-q-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal
             qstr "=?UTF-8?Q?=D9=84=DB=8C=D9=86=DA=A9=20=D8=AA=D8=A7=DB=8C=DB=8C=D8=AF=20=7C=20=56=65=72=69=66=69=63=61=74=69=6F=6E=20=6C=69=6E=6B?="))))

(define-cl-smtp-test "rfc2045-q-encode-string-utf-8-3" ()
  (let* ((str "Test: check correct newline in quoted encoding string for string longer then 74 charecters with spezial german 'ü' 'Ü' 'ä' 'Ä' 'ö' 'Ö' 'ß' characters")
         (qstr (rfc2045-q-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (equal
             qstr "=?UTF-8?Q?=54=65=73=74=3A=20=63=68=65=63=6B=20=63=6F=72=72=65=63=74=20=6E=65=77=6C=69=6E=65=20=69=6E=20=71=75=6F=74=65=64=20=65=6E=63=6F=64=69=6E=67=20=73=74=72=69=6E=67=20=66=6F=72=20=73=74=72=69=6E=67=20=6C=6F=6E=67=65=72=20=74=68?=
 =?UTF-8?Q?=65=6E=20=37=34=20=63=68=61=72=65=63=74=65=72=73=20=77=69=74=68=20=73=70=65=7A=69=61=6C=20=67=65=72=6D=61=6E=20=27=C3=BC=27=20=27=C3=9C=27=20=27=C3=A4=27=20=27=C3=84=27=20=27=C3=B6=27=20=27=C3=96=27=20=27=C3=9F=27=20=63=68=61=72=61=63=74=65=72?=
 =?UTF-8?Q?=73?="))))

(define-cl-smtp-test "rfc2045-q-encode-string-newline-1" ()
  (assert (equal (rfc2045-q-encode-string
                  "123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A")
                 "123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A")))

(define-cl-smtp-test "rfc2045-q-encode-string-newline-2" ()
  (assert (equal (rfc2045-q-encode-string
                  (format nil "123456789A123456789A~CEND"
                          #\Newline))
                 (format nil "123456789A123456789A~C~C END"
                         #\Return #\Newline))))

(define-cl-smtp-test "rfc2045-q-encode-string-newline-3" ()
  (assert (equal (rfc2045-q-encode-string
                  (format nil "123456789A123456789A~C~C.~C~CEND"
                          #\Return #\Newline #\Return #\Newline))
                 (format nil "123456789A123456789A~C~C .~C~C END"
                          #\Return #\Newline #\Return #\Newline))))

(define-cl-smtp-test "rfc2045-q-encode-string-newline-4" ()
  (assert (equal (rfc2045-q-encode-string
                  "123456789A 123456789A 123456789A 123456789A 123456789A 123456789A 123456789A END")
                 (format nil "123456789A 123456789A 123456789A 123456789A 123456789A 123456789A 123456789A~C~C END"
                         #\Return #\Newline))))

(define-cl-smtp-test "escape-rfc822-quoted-string" ()
  (assert (equal (escape-rfc822-quoted-string "test end") "test end"))
  (assert (equal (escape-rfc822-quoted-string "test\\end") 
                 "test\\\\end"))
  (assert (equal (escape-rfc822-quoted-string "test\"end")
                 "test\\\"end"))
  (assert (equal (escape-rfc822-quoted-string (format nil "test~%end")) 
                 (format nil "test\\~%end")))
  (assert (equal (escape-rfc822-quoted-string 
                  (format nil "test~cend" #\Return)) 
                 (format nil "test\\~cend" #\Return)))
  (assert (equal (escape-rfc822-quoted-string "test/end") "test/end"))
  (assert (equal (escape-rfc822-quoted-string "test end\\") 
                 "test end\\\\"))
  (assert (equal (escape-rfc822-quoted-string (format nil "~%test end\\")) 
                 (format nil "\\~%test end\\\\"))))

(define-cl-smtp-test "rfc2231-encode-string-utf-8" ()
  (let* ((str "öüäÖÜÄß")
         (qstr (rfc2231-encode-string str :external-format :utf-8)))
    (assert qstr)
    (assert (string-equal 
             qstr "UTF-8''%C3%B6%C3%BC%C3%A4%C3%96%C3%9C%C3%84%C3%9F"))
    ))

(define-cl-smtp-test "make-attachment-1" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p)))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "make-attachment-2" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment p))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "make-attachment-3" ()
  (let* ((p (namestring (merge-pathnames "tests.lisp" 
                                         (get-component-pathname))))
         (attachment p))
    (assert (equal (attachment-name attachment) (file-namestring p)))
    (assert (equal (attachment-mime-type attachment) "text/plain"))
    (assert (equal (attachment-data-pathname attachment) p))
    ))

(define-cl-smtp-test "send-attachment-header-1" ()
  (let* ((boundary (make-random-boundary))
         (p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p))
         (headerstr (with-output-to-string (s)
                      (send-attachment-header s boundary attachment :utf-8)))
         (returnnewline (format nil (format nil "~C~C" #\Return #\NewLine)))
         (tmpstr (format nil "--~A~AContent-type: text/plain;~% name*=UTF-8''tests.lisp;~% name=\"tests.lisp\"~AContent-Disposition: attachment; filename*=UTF-8''tests.lisp; filename=\"tests.lisp\"~AContent-Transfer-Encoding: base64~A~A" 
                         boundary returnnewline returnnewline returnnewline 
                         returnnewline returnnewline)))
    (assert (equal headerstr tmpstr))
    ))

(define-cl-smtp-test "send-attachment-header-2" ()
  (let* ((boundary (make-random-boundary))
         (p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (attachment (make-attachment p
				      :mime-type "text/plain"
				      :name "foo\\bar"))
         (headerstr (with-output-to-string (s)
                      (send-attachment-header s boundary attachment :utf-8)))
         (returnnewline (format nil (format nil "~C~C" #\Return #\NewLine)))
         (tmpstr (format nil "--~A~AContent-type: text/plain;~% name*=UTF-8''foo%5Cbar;~% name=\"foo\\\\bar\"~AContent-Disposition: attachment; filename*=UTF-8''foo%5Cbar; filename=\"foo\\\\bar\"~AContent-Transfer-Encoding: base64~A~A" 
                         boundary returnnewline returnnewline returnnewline 
                         returnnewline returnnewline)))
    (assert (equal headerstr tmpstr))
    ))


(define-cl-smtp-test "mask-dot-1" ()
  (assert (equal (mask-dot (format nil "~C~C.~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C..~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C~C..~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C..~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C~C~C~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C~C~C~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C.~C.~C.~C" #\Return #\NewLine
                                   #\Return #\NewLine))
                 (format nil "~C.~C..~C.~C" #\Return #\NewLine
                         #\Return #\NewLine)))
  (assert (equal (mask-dot (format nil "~C.~C" #\NewLine #\NewLine))
                 (format nil "~C..~C"  #\NewLine #\NewLine)))
  (assert (equal (mask-dot (format nil ".~C.~C" #\NewLine #\NewLine))
                 (format nil "..~C..~C"  #\NewLine #\NewLine))))

(define-cl-smtp-test "substitute-return-newline" ()
  (assert (equal (substitute-return-newline 
                  (format nil "start~Aende" *return-newline*))
                 "start ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~Aweiter~Aende" 
                          *return-newline* *return-newline*))
                 "start weiter ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "~Astart~Aweiter~Aende~A" 
                          *return-newline* *return-newline* *return-newline*
                          *return-newline*))
                 " start weiter ende "))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~Aende" 
                          *return-newline* *return-newline*))
                 "start  ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~A~Aende" 
                          *return-newline* *return-newline* *return-newline*))
                 "start   ende"))
  (assert (equal (substitute-return-newline 
                  (format nil "start~A~%~A~Aende" 
                          *return-newline* *return-newline* *return-newline*))
                 "start 
  ende"))
  )

(defun file-to-usb8-buffer (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let* ((flength (file-length s))
           (buffer (make-array flength :element-type '(unsigned-byte 8))))
      (loop for i from 0 to flength do
           (let ((bchar (read-byte s nil 'EOF)))
             (if (eql bchar 'EOF)
                 (return)
                 (setf (aref buffer i) bchar))))
      buffer)))

(define-cl-smtp-test "base64-encode-file" ()
  (let* ((p (merge-pathnames "tests.lisp" (get-component-pathname)))
         (base64str1 (with-output-to-string (s)
                       (base64-encode-file p s)))
         (buffer (file-to-usb8-buffer p))
         (base64str2 
          #-allegro
           (cl-base64:usb8-array-to-base64-string buffer :columns 0)
          #+allegro 
          (excl:usb8-array-to-base64-string buffer :wrap-at-column nil)
           )) 
    
    (assert (string-equal (remove #\Return (remove #\Newline base64str1 :test #'equal) :test #'equal) base64str2))
    ))

(defun run-test (name &optional (catch-errors t))
  (handler-case
      (let ((test (gethash name *cl-smtp-tests*)))
        (format t "~%run test: ~S ~@[(~A)~]~%" name (cadr test))
        (apply (car test) (cadr test))
        (format t "pass~%")
        t)
    (simple-error (c)
      (if catch-errors
          (format t "failed: ~A" c)
          (error c)))))

(defun run-tests (&optional (catch-errors t))
  (let ((n (hash-table-count *cl-smtp-tests*))
        (failed '())
        (pass 0))
    (format t "~%run ~D cl-smtp-tests~%~%" (hash-table-count *cl-smtp-tests*))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (if (run-test k catch-errors)
                     (incf pass)
                     (push k failed)))
             *cl-smtp-tests*)
    (format t "~%pass: ~D | failed: ~D~%test failed: ~{~A~^, ~}~%~%" 
            pass (- n pass) failed)))
