;;
;; Copyright 2002-2012 Genworks International
;;
;; Except otherwise noted in documentation below, this source file is
;; part of the General-purpose Declarative Language project (GDL).
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


(in-package :gdl)

(defmacro with-error-handling ((&key (error? nil)
				     (timeout nil) 
                                     (timeout-body 
                                      `(warn "Timed Out after ~a Seconds" ,timeout))) 
                               &body body)
  "[Macro]. Wraps the <b>body</b> of code with error-trapping and system timeout. 
A warning is given if an error condition occurs with <b>body</b>. 

:&key ((timeout 2) \"Timeout in Seconds.\"
          timeout-body \"Body of code to evaluate if timeout occurs. 
                         Default is to print a warning and return nil.\")

:&rest (body \"Body of code to be wrapped\")"
  
  (declare (ignore timeout-body))
  
  (if error? `(progn ,@body)
    (let ((values '+values+ #+nil (gensym)) (error '+error+ #+nil (gensym)))
      (let ((code `(let* ((,values (multiple-value-list (ignore-errors ,@body)))

                          (,error (second ,values)))
                     (if (and ,error (typep ,error 'error))
                         (progn (warn "~a" ,error)
                           (values nil ,error))
                       (apply #'values ,values)))))
        (if timeout 
            (error "Need implementation for timeout in with-error-handling.~%")
          code)))))



(defun read-safe-string (string)
  "Lisp object. Reads an item from string, protecting against lisp evaluation with 
the `#.' reader macro. Throws an error if evaluation would have occured.

:arguments (string \"string\")

"
  (if (not (stringp string)) string
    (when (not (zerop (length string)))
      (let ((*read-eval* nil))
        (read-from-string string)))))

(defun alist2plist (alist)
  "Plist. Converts an assoc-list to a plist.

:arguments (alist \"Assoc-List\")
"
  
  (when alist
    (cons (first (first alist))
          (cons
           (second (first alist))
           (alist2plist (rest alist))))))


(defun replace-substring (string old new)
  "String. Replaces all substring occurrences of <b>old</b> with <b>new</b>
in <b>string</b>.

Note: In a full GDL system, you will have glisp:replace-regexp, which
      is more powerful and probably more efficient than this.

:arguments (string \"String. The source string.\"
            old \"String. The substring to be replaced.\"
            new \"String. The substring to replace it with.\")

:see-also <tt>excl:replace-regexp</tt>

"
  
  (let ((position (search old string)))
    (if position
        (string-append 
         (subseq string 0 position) new
         (replace-substring (subseq string (+ position (length old))) old new))
      string)))

(defun lastcar (list)
  "Lisp Object. Returns the last element of <b>list</b>.

:arguments (list \"List\")"
  
  (first (last list)))


(defun mapsend (object-list message &rest args)
  "List. Returns a new list which is the result of sending <b>message</b> to each GDL 
object in <b>object-list</b>.

:arguments (object-list \"List of GDL objects.\"
            message \"Keyword symbol.\")"

  (declare (ignore args))
  
  (let ((object-list (if (typep object-list 'quantification)
                         (list-elements object-list)
                       object-list)))
    (mapcar #'(lambda(object)
                (the-object object (evaluate message))) object-list)))


(defun list-of-n-numbers (num1 num2 n)
  "Returns a list of n numbers equally spaced between bounds num1 and num2, inclusive."
  (let ((increment (/ (- num2 num1) (- n 1)))
    (result-list nil))
    (dotimes (i (- n 1) (nreverse (cons num2 result-list)))
      (push (+ num1 (* i increment)) result-list))))


(defun list-of-numbers (num1 num2 &optional (increment 1))
  "List of Numbers. Returns a list of incrementing numbers starting from 
<b>num1</b> and ending with <b>num2</b>, inclusive.

:arguments (num1 \"Number\"
            num2 \"Number\")
:&optional ((increment 1) \"Number. The distance between the returned listed numbers.\")"

  (let ((result-list nil))
    (dotimes (i (1+ (floor (/ (- num2 num1) increment))) (nreverse result-list))
      (push (+ (* i increment) num1) result-list))))


(defun half (num)
  "Number. Returns the result of dividing <b>num</b> by the integer <tt>2</tt>.
The type of the returned number will depend on the type of <b>num</b>.

:arguments (num \"Number\")"
  (let ((result (/ num 2)))
    (if *bias-to-double-float?* (to-double-float result) result)))

(defun twice (num)
  "Number. Returns the result of multiplying <b>num</b> by the integer <tt>2</tt>.
The type of the returned number will depend on the type of <b>num</b>.

:arguments (num \"Number\")"
  
  (let ((result (+ num num)))
    (if *bias-to-double-float?* (to-double-float result) result)))


(defun index-filter (fn list)
  "List. Returns all elements of <b>list</b> for whose index (starting at zero) the 
function <b>fn</b> returns non-NIL.

:arguments (fn \"Function object (e.g. a lambda expression)\"
            list \"List\")"
  
  (let ((count 0)
        (result-list nil))
    (dolist (elem list (nreverse result-list))
      (when (funcall fn count) (push elem result-list))
      (incf count))))


(defun flatten (tree &aux result)
  "List. Returns a new list consisting of only the leaf-level atoms from <b>list</b>. 
Since nil is technically a list, <tt>flatten</tt> also has the effect of removing 
nils from <b>list</b>, but may be inefficient if used only for this purpose. For 
removing nil values from a list, consider using <tt>remove nil ...</tt> instead.

:note from Stack Overflow forum:  http://stackoverflow.com/questions/25866292/flatten-a-list-using-common-lisp

:note Creative Commons license

<pre>
 (defun flatten (lst &aux (result '()))
   (labels ((rflatten (lst1)
              (dolist (el lst1 result)
                (if (listp el)
                  (rflatten el)
                  (push el result)))))
       (nreverse (rflatten lst))))
  </pre>

:note This will not work with dotted lists, only with actual lists. If
you need dotted lists, use the old definition of flatten, from Paul
Graham On Lisp:

<pre>

  (defun flatten (tree)
   (if (atom tree)
       (ensure-list tree)
       (nconc (flatten (car tree))
              (if (cdr tree) (flatten (cdr tree))))))
</pre>

:arguments (list \"List\")
:see-also <tt>remove</tt>"

  (labels ((rflatten (tree1)
	     (dolist (el tree1 result)
	       (if (listp el)
		   (rflatten el)
		   (push el result)))))
    (nreverse (rflatten tree))))


(defun split (string &optional (split-chars (list #\space #\newline #\return #\tab)))
  
  "List of Strings. Returns a list containing the elements of <b>string</b> after having
been split according to <b>split-chars</b> as delimiting characters.

:arguments (string \"String\")
:&optional ((split-chars (list #\\space #\\newline #\\return #\\tab)) \"List of characters\")

:see-also <tt>glisp:split-regexp</tt>"
  
  (let ((split-chars (if (atom split-chars) (list split-chars) split-chars)))
    (let ((non-white
           (position nil string :test #'(lambda(item1 item2)
                                          (declare (ignore item1))
                                          (not (member item2 split-chars))))))
      (let ((newlines-before (count #\newline (subseq string 0 non-white))))
        (append (make-list newlines-before :initial-element :newline)
                (when non-white
                  (let ((white
                         (position nil (subseq string non-white)
                                   :test #'(lambda(item1 item2)
                                             (declare (ignore item1))
                                             (member item2 split-chars)))))
                    (cons (subseq string non-white (when white (+ white non-white)))
                          (when white (split (subseq string (+ non-white white)) split-chars))))))))))


(defun always (arg)
  "T. Always returns the value <tt>T</tt> regardless of <b>arg</b>.

:arguments (arg \"Lisp object. Ignored\")"
  
  (declare (ignore arg)) t)

(defun never (arg)
  "NIL. Always returns the value <tt>NIL</tt> regardless of <b>arg</b>.

:arguments (arg \"Lisp object. Ignored\")"
  (declare (ignore arg)))




;;
;; FLAG -- creates too many lists look into using some kind of
;;         sequence composition or series package
;;
(defun maptree (node fn &optional (accept? #'always)
                                  (prune?  #'never)
                                  (get-children :children))
  "List. Returns the results of applying <b>fn</b> to each GDL object in the object 
tree rooted at <b>node</b> in a ``depth-first'' tree traversal.

:arguments (node \"GDL object\"
            fn \"Function. Operates on a single argument which is a GDL object\")
:&optional ((accept? #'always) \"Function. Determines which nodes to accept in the final result\"
            (prune? #'never) \"Function. Determines which nodes to prune from the tree traversal\"
            (get-children :children) \"Keyword symbol :children or Function. Function applied to 
a given node to get its children. The default, keyword symbol :children, uses the node's normal
children as returned by (the-object node children).\")"

  (let ((object-list (flatten (traverse-tree node prune? get-children))))
    (mapcar fn (remove-if-not accept? object-list))))


(defun traverse-tree (node &optional prune? (get-children :children))
  (cons  node
         (when (not (funcall prune? node))         
           (mapcar #'(lambda(child)
                       (traverse-tree child prune? get-children))
                   (cond ((eql get-children :children)
                          (the-object node :children))
                         ((keywordp get-children)
                          (the-object node (evaluate get-children)))
                         ((functionp get-children)
                          (funcall get-children node))
                         (t (error
			     "
Get-Children arg of ~s Not Handled by traverse-tree" node)))))))


(defun hash-table-copy (ht)
  (let ((new-hash (make-hash-table :size (hash-table-count ht) :test (hash-table-test ht))))
    (maphash #'(lambda(key val) (setf (gethash key new-hash) val)) ht) new-hash))

(defun hash-table-compare (ht1 ht2 &key (test 'equalp))
  (unless (and (= (hash-table-size ht1) (hash-table-size ht2)) 
               (eql (hash-table-test ht1) (hash-table-test ht2)))
    (error "For hash-table-compare, sizes and tests for hash tables must be the same"))
  (let ((new-hash (make-hash-table :size (hash-table-size ht1) :test (hash-table-test ht1))))
    (maphash #'(lambda(key val1) 
                 (let ((val2 (gethash key ht2)))
                   (setf (gethash key new-hash) (if (funcall test val1 val2) 'gdl-rule::%same% val1)))) 
             ht1) new-hash))
  
  



(defun expandit (form &optional (stream *standard-output*))
  (let ((*print-case* :downcase)
        (*print-right-margin* 60))
    (if (streamp stream)
        (pprint (macroexpand-1 (macroexpand-1 form)) stream)
      (with-open-file (stream stream :direction :output :if-exists :supersede :if-does-not-exist :create)
        (pprint (macroexpand-1 (macroexpand-1 form)) stream)))))
                    


(defun undefine-object (object-name-symbol)
  "NIL. Clears all definitions associated with <b>object-name</b> from the 
currently running GDL session.

:arguments (object-name \"Non-keyword Symbol naming a GDL object type\")"
  
  ;;
  ;; FLAG -- repeated code from define-object -- try to factor out.
  ;;
  (when (find-class object-name-symbol nil)
    (let ((message-keys (get object-name-symbol :%gdl-messages%)))
      (dolist (key message-keys)
        (let ((method (find-method (symbol-function (glisp:intern (symbol-name key) :gdl-slots)) nil
                                   (list (find-class object-name-symbol)) nil)))
          (if (null method)
              (warn "No Method Found for Message ~s of object ~s~%" key object-name-symbol)
            (remove-method (symbol-function (glisp:intern (symbol-name key) :gdl-slots)) method)))))
    (setf (find-class object-name-symbol) nil))
  (setf (get object-name-symbol :%gdl-messages%) nil))


(defun safe-sort (list &rest args)
  "List. Nondestructive analog of the Common Lisp <tt>sort</tt> function. 
Returns a freshly created list.

:arguments (list \"List. The list to be sorted.\")
:&rest (args \"Argument list. Identical to the arguments for Common Lisp <tt>sort</tt>.\")"
    
  (let ((copy (copy-list list)))
    (apply #'sort copy args)))


(defun gdl-object-symbol? (symbol) (eql (class-of (find-class symbol nil)) (find-class 'gdl-class)))
(deftype gdl-object-symbol () '(satisfies gdl-object-symbol?))

(defun gdl-format-symbol? (symbol) (eql (class-of (find-class symbol nil)) (find-class 'gdl-format-class)))
(deftype gdl-format-symbol () '(satisfies gdl-format-symbol?))

;;
;; From Paul Graham ACL
;;
(defun most (function list)
  "List. Returns the member of <b>list</b> which returns the maximum numerical value
when <b>function</b> is applied to it. As second value is returned which is the
actual maximum value (the return-value of <b>function</b> as applied). This function 
comes from the Paul Graham book <u>ANSI Common Lisp</u>.

:arguments (function \"Function\"
            list \"List\")"

  (if (null list)
      (values nil nil)
    (let* ((wins (first list))
           (max (funcall function wins)))
      (dolist (obj (rest list))
        (let ((score (funcall function obj)))
          (when (> score max)
            (setq wins obj max score))))
      (values wins max))))



(defun least (function list)
  "List. Returns the member of <b>list</b> which returns the minimum numerical value
when <b>function</b> is applied to it. As second value is returned which is the
actual minimum value (the return-value of <b>function</b> as applied). This function 
comes from the Paul Graham book <u>ANSI Common Lisp</u>.

:arguments (function \"Function\"
            list \"List\")"

  (if (null list)
      (values nil nil)
    (let* ((wins (first list))
           (min (funcall function wins)))
      (dolist (obj (rest list))
        (let ((score (funcall function obj)))
          (when (< score min)
            (setq wins obj min score))))
      (values wins min))))


(defun ^2  (number)
  "Number. Return <b>number</b> raised to the power two (2).

:arguments (number \"Number\")"
  
  (* number number))


(defun number-round (number &optional decimal-places)
  "Number. Returns <b>number</b> rounded to <b>decimal-places</b> decimal places.

:arguments (number \"Number\"
            decimal-places \"Integer\")"

  (if (null decimal-places)
      number
    (coerce (/ (round (* number (expt 10 decimal-places))) (expt 10 decimal-places)) 'double-float)))

(defun number-format (number &optional decimal-places)
  "String. Returns a string displaying <b>number</b> rounded to <b>decimal-places</b> decimal places.

:arguments (number \"Number\"
            decimal-places \"Integer\")"
  
  (let ((rep (number-round number decimal-places)))
    (if (and (or (null decimal-places) (zerop decimal-places)) (= rep (truncate rep))) (format nil "~a" (truncate rep))
      (format nil (format nil "~~,~af" decimal-places) (coerce rep 'double-float)))))

;;
;; FLAG -- handle points of 2 and 4 dimensions.
;;


(defun near-to? (number near-to &optional (tolerance *zero-epsilon*))
  "Boolean. Predicate to test if number is within tolerance of 
<b>near-to</b>. The default tolerance is the value of the parameter 
<tt>*zero-epsilon*</tt>.

:arguments (number \"Number\"
            near-to \"Number\")
:&optional ((tolerance *zero-epsilon*) \"Number\")"

  (< (abs (- number near-to)) tolerance))

(defun near-zero? (number &optional (tolerance *zero-epsilon*))
  "Boolean. Returns non-NIL iff <b>number</b> is greater than <b>tolerance</b> different
from zero.

:arguments (number \"Number\")
:&optional ((tolerance *zero-epsilon*) \"Number\")
:see-also <tt>zerop</tt> (Common Lisp function)"
             
  (near-to? number 0 tolerance))


(defun status-message (string)
  "NIL. Prints <b>string</b>, followed by a newline, to <tt>*trace-output*</tt>, 
which is generally the system console.

:arguments (string \"String\")"

  (write-string string *trace-output*)
  (format *trace-output* "~%"))



(defun safe-float (number)
  "Double-float Number. Coerces <b>number</b> to a double-precision floating-point
number if possible. If this is not possible, returns <tt>0.0d0</tt> (i.e. zero in the form
of a double-precision floating-point number).

:arguments (number \"Number\")"
  
  (if (typep number '(or integer float ratio))
      (coerce number 'double-float) 
    0.0d0))


(defun round-to-nearest (number interval)
  "Number. Rounds <b>number</b> to the nearest <b>interval</b>.

:arguments (number \"Number\"
            interval \"Number\")"
  ;;(* (round number interval) interval)
  
  (/ (round number interval) (/ interval))
  )


(defun fround-to-nearest (number interval)
  "Number. Rounds <b>number</b> to the nearest <b>interval</b>,
using type contagion rules for floating-point similar to the 
CL \"fround\" function.

:arguments (number \"Number\"
            interval \"Number\")"
  
  (* (fround number interval) interval))


(defun find-all-superclasses (class &optional (local? nil))
  (let ((directs (glisp:direct-superclasses class)))
    (if local? directs
      (remove-duplicates 
       (flatten (mapcan #'(lambda(direct)
                            (list direct (find-all-superclasses direct))) 
                        directs)) :from-end t))))

(defun symbols-as-keywords (item)
  (when item
    (cond ((symbolp item) (make-keyword item))
          ((listp item) (mapcar #'symbols-as-keywords item))
          (t item))))
        

(defmethod readable-expression ((object t) &optional self) 
  (declare (ignore self))
  (error "Objects of type ~a cannot currently be written out and read back into GDL reliably.
 Please do not use objects of this type as the value for toplevel inputs or settable slots, 
 or contact Genworks with an enhancement request to include a readable output format for 
 this object type." (type-of object)))


(defmethod readable-expression ((object hash-table) &optional self) 
  (let (keys vals)
    (maphash #'(lambda(key val) (push key keys) (push val vals)) object)
    `(let ((ht (make-hash-table)))
       (mapc #'(lambda(key val)
		 (setf (gethash key ht) val))
	     ,(readable-expression (nreverse keys) self)
	     ,(readable-expression (nreverse vals) self)) ht)))

(defmethod readable-expression ((object cons) &optional self)  
  
  (with-error-handling (:timeout nil) `(list ,@(mapcar #'(lambda(expr)
							   (readable-expression expr self))
						       object))))

(defmethod readable-expression ((object number) &optional self) (declare (ignore self)) object)
(defmethod readable-expression ((object string) &optional self) (declare (ignore self)) object)
(defmethod readable-expression ((object symbol) &optional self) (declare (ignore self)) 
           `',object)
(defmethod readable-expression ((object array) &optional self) (declare (ignore self)) object)
(defmethod readable-expression ((object pathname) &optional self)
  (declare (ignore self)) object)

(defmethod lookup-color ((color string) &key (format :decimal) (ground :foreground))
  (if (and (eql (aref color 0) #\#) (= (length color) 7))
      (ecase format 
        (:hex color)
        (:decimal (let ((red (subseq color 1 3))
                        (green (subseq color 3 5))
                        (blue (subseq color 5 7)))
                    (let* ((*read-base* 16) 
                           (red (to-single-float (div (read-safe-string red) 255)))
                           (green (to-single-float (div (read-safe-string green) 255)))
                           (blue (to-single-float (div (read-safe-string blue) 255))))
                      (values (make-array 3 :initial-contents (list red green blue)) t)))))
    
    (progn
      (when (= (length color) 6)
        (setq color (string-append "#" color)))

      (ecase format 
        (:decimal (multiple-value-bind (decimal found?)  (gethash color *color-table-decimal*)
                    (values (coerce (or decimal (lookup-color (getf *colors-default* ground) :format :decimal)) 'vector) found?)))
        (:hex     (if (eql (aref color 0) #\#) (values color t)
                    (multiple-value-bind (hex found?) (gethash (make-keyword color) *color-table*)
                      (values (or hex (lookup-color (getf *colors-default* ground) :format :hex)) found?))))))))

(defmethod lookup-color ((color symbol) &key (format :decimal) (ground :foreground))
  (ecase format 
    (:decimal (multiple-value-bind (decimal found?)  (gethash color *color-table-decimal*)
                (values (coerce (or decimal (lookup-color (getf *colors-default* ground) :format :decimal)) 'vector) found?)))
    (:hex (multiple-value-bind (hex found?) (gethash (make-keyword color) *color-table*)
            (values (or hex (lookup-color (getf *colors-default* ground) :format :hex)) found?)))))


(defmethod lookup-color ((color vector) &key (format :decimal) (ground :foreground))
  (declare (ignore ground))
  (ecase format 
    (:decimal (values (map 'vector #'to-single-float color)  t))
    (:hex (setq color (make-array 3 :initial-contents (list (truncate (* (aref color 0) 255))
                                                            (truncate (* (aref color 1) 255))
                                                            (truncate (* (aref color 2) 255)))))
          (string-upcase (let ((*print-base* 16)) (values (format nil "#~2,,,'0@a~2,,,'0@a~2,,,'0@a" 
                                                                  (aref color 0)(aref color 1)(aref color 2)) t))))))



(defmethod lookup-color ((color cons) &key (format :decimal) (ground :foreground))
  (declare (ignore ground))
  (ecase format 
    (:decimal (values (coerce (mapcar #'to-single-float color) 'vector) t))
    (:hex (let ((*print-base* 16)) (values (format nil "#~2,,,'0@a~2,,,'0@a~2,,,'0@a" 
                                                   (truncate (* (first color) 255))
                                                   (truncate (* (second color) 255))
                                                   (truncate (* (third color) 255))) t)))))


;;
;; FLAG -- remove this defunct version. 
;;
#+nil
(defmethod lookup-color ((color null) &key (format :decimal) (ground :foreground))
  (unless (and (eql ground :foreground)
	       (eql (getf *colors-default* :foreground) :black))
    (values (lookup-color (getf *colors-default* ground) :format format) nil)))


(defmethod lookup-color ((color null) &key (format :decimal) (ground :foreground))
  (values (lookup-color (getf *colors-default* ground) :format format) nil))
     
                
(defun cyclic-nth (number list)
  "Lisp object. Returns nth from the list, or wraps around if nth is greater than the length of the list."
  (nth (mod number (length list)) list))



(defun set-self (object)
  (setf (symbol-value 'self) object))


(eval-when (compile load eval) (export 'with-error-handling))
                
                
                
(defmethod evaluate-object ((category t) args)
  (declare (ignore args))
  nil)


(defun iso-8601-date (universal-time &key include-time?)
  "String. Returns the ISO8601 formatted date and possibly time from a
Common Lisp universal time integer, e.g. 2007-11-30 or
2007-11-30T13:45:10" 
  (multiple-value-bind (seconds minutes hours date month year day daylight-savings? timezone)
      (decode-universal-time universal-time)
    (declare (ignore day daylight-savings? timezone))
    (format nil "~a-~2,,,'0@a-~2,,,'0@a~a" year month date
            (if include-time? (format nil "T~2,,,'0@a:~2,,,'0@a:~2,,,'0@a" hours minutes seconds) ""))))

(defun universal-time-to-plist (universal-time)
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time universal-time)
    (list :seconds (format nil "~2,,,'0@a" seconds)
	  :minutes (format nil "~2,,,'0@a" minutes)
	  :hours (format nil "~2,,,'0@a" hours)
	  :date (format nil "~2,,,'0@a" date)
	  :month (format nil "~2,,,'0@a" month)
	  :year (format nil "~a" year)
	  :full-date (format nil "~a-~2,,,'0@a-~2,,,'0@a" year month date))))


(defun universal-time-from-iso-8601 (iso8601-date)
  "Integer representing Common Lisp Universal Time. Returns the universal time from a date formatted as 
an iso-8601 date, optionally with time, e.g. 2012-07-08 or 2012-07-08T13:33 or 2012-07-08T13:33:00"
  (destructuring-bind (date &optional (time "00:00:00"))
      ;;(glisp:split-regexp "t" (string-downcase iso8601-date))
      (split (string-downcase iso8601-date) #\t)
    (destructuring-bind  (year month date) 
	;;(glisp:split-regexp "-" date)
	(split date #\-)
      (destructuring-bind (hours minutes &optional (seconds "00"))
	  ;;(glisp:split-regexp ":" time)
	  (split time #\:)
	(encode-universal-time (parse-integer seconds)
			       (parse-integer minutes)
			       (parse-integer hours)
			       (parse-integer date)
			       (parse-integer month)
			       (parse-integer year))))))

(defun print-hash (hash) 
  (maphash #'(lambda(key val) (print-variables key val)) hash))

(defun list-hash (hash) 
  (let (result)
    (maphash #'(lambda(key val)
                 (if result (nconc result (list key val))
                   (setq result (list key val)))) hash) result))


(defun make-fresh-copy (&key object make-object-args)
  
  (apply #'make-object (the-object object type) make-object-args))
  



(defun read-snapshot (&key (filename "/tmp/snap.gdl") 
			string stream
			object keep-bashed-values? make-object-args 
			keys-to-ignore
			(keys-to-ignore-default (list :query-plist :view-toggle :cookies-received)))
    
  "GDL Instance. Reads the snapshot data from stream, from the string,
or from file indicated by filename. If no optional keyword
<tt>object</tt> argument is given, a new GDL instance based on the
data in the snapshot file is returned. If an <tt>object</tt> is given,
the object should be compatible in type to that specified in the
snapshot file, and this existing object will be modified to contain
the set slot values and toplevel inputs as specified in the snapshot
file.

:&key ((filename \"/tmp/snap.gdl\") \"String or pathname. File to be read. If either string or stream is specified, this will not be used.\"
       (string nil) \"String of data. The actual snapshot contents, stored in a string. If stream is specified, this will not be used.\"
       (stream nil) \"Stream open for input. A stream from which the snapshot data can be read.\"
       (keep-bashed-values? nil) \"Boolean. Indicates whether to keep the currently bashed values in object before reading snap values into it.\"
       (object nil) \"GDL object. Existing object to be modified with restored values.\")"
  
  (let ((keys-to-ignore (append keys-to-ignore keys-to-ignore-default)))
    

    (cond (stream (read-snapshot-from-stream stream :object object :keep-bashed-values? keep-bashed-values?
					     :make-object-args make-object-args :keys-to-ignore keys-to-ignore))
	  (string (with-input-from-string (in string)
		    (read-snapshot-from-stream in :object object :keep-bashed-values? keep-bashed-values?
					       :make-object-args make-object-args :keys-to-ignore keys-to-ignore)))
	  (filename (with-open-file (in filename)
		      (read-snapshot-from-stream in :object object :keep-bashed-values? keep-bashed-values?
						 :make-object-args make-object-args :keys-to-ignore keys-to-ignore))))))




(defun read-snapshot-from-stream (in &key object keep-bashed-values? make-object-args keys-to-ignore)

  (let ((package-form (read in)))
	(when (or (null package-form) (not (find-package (second package-form))))
	  (error "Invalid package specification at beginning of stream ~s.~%" in))
	(let* ((*package* (find-package (second package-form))) (root-form (read in)))
	  (when (or (null root-form) 
		    (not (eql (class-of (find-class (first root-form))) (find-class 'gdl-class))))
	    (error "Invalid object type specifier as first element of second form in ~a.~%" root-form))

        
	  (let* ((object (cond ((and object keep-bashed-values?) object)
			       (object (the-object object restore-tree!) object)
			       (t (progn
				    (apply #'make-object (first root-form) make-object-args))))))

	    
	    (let ((self object) (value-plist (rest root-form)))

	      (mapc #'(lambda(key expression) 
			(unless (member key keys-to-ignore)
			  (when self (the-object self (set-slot! key (eval expression) :re-sort? nil)))))
		    (plist-keys value-plist) (plist-values value-plist)))
          
	    (let (forms)
	      (do ((form (read in nil nil) (read in nil nil)))
		  ((null form) forms)
		(push form forms))
	    
	      (setq forms (nreverse forms))
	    
	      (let* ((forms (double-length-sort forms))
		     
		     (primaries (remove-if-not #'(lambda(item) (or (getf (rest item) :%primary?%)
								   (getf (rest item) :element-index-list))) forms))
		     (non-primaries (remove-if #'(lambda(item) (or (getf (rest item) :%primary?%)
								   (getf (rest item) :element-index-list))) forms))
		     (forms (append primaries non-primaries)))
              
		(dolist (form forms)
            
		  (let ((root-path (first form)) (value-plist (rest form)))
		    (let ((self 
			   (with-error-handling (:timeout nil)
			     (the-object object (follow-root-path root-path)))))
		      (when self
			(mapc #'(lambda(key expression) 
				  (unless (member key keys-to-ignore)
				    (the-object self (set-slot! key (eval `(let ((self ,self)) ,expression)) :re-sort? nil))))
			      (plist-keys value-plist) (plist-values value-plist))))))))
	    object))))




#+nil
(defun read-snapshot-from-stream (in &key object keep-bashed-values? make-object-args keys-to-ignore)

  (let (forms)
    (do ((form (read in nil nil) (read in nil nil)))
	((null form) forms)
      (push form forms))
    
    (setq forms (nreverse forms))
    
    (let ((package-form (first forms)))
      (when (or (null package-form) (not (find-package (second package-form))))
	(error "Invalid package specification at beginning of snapshot data.~%"))
      (let* ((*package* (find-package (second package-form))) 
	     (root-form (second forms)))
	(when (or (null root-form) 
		  (not (atom (first root-form)))
		  (not (eql (class-of (find-class (first root-form))) (find-class 'gdl-class))))
	  (warn "Invalid root-form: ~a.~%" root-form)
	  
	  (setq root-form (first (remove-if-not #'(lambda(form) (atom (first form))) (rest forms))))
	  (setq forms (cons package-form
			    (cons root-form
				  (remove root-form (rest forms) :test #'equalp)))))
			    
        
	  (let* ((object (cond ((and object keep-bashed-values?) object)
			       (object (the-object object restore-tree!) object)
			       (t (progn
				    (apply #'make-object (first root-form) make-object-args))))))
	    
	    (let ((self object) (value-plist (rest root-form)))

	      (mapc #'(lambda(key expression) 
			(unless (member key keys-to-ignore)
			  (when self (the-object self (set-slot! key (eval expression) :re-sort? nil)))))
		    (plist-keys value-plist) (plist-values value-plist)))
          
	    
	    (let* ((forms (double-length-sort forms))
		     
		   (primaries (remove-if-not #'(lambda(item) (or (getf (rest item) :%primary?%)
								 (getf (rest item) :element-index-list))) forms))
		   (non-primaries (remove-if #'(lambda(item) (or (getf (rest item) :%primary?%)
								 (getf (rest item) :element-index-list))) forms))
		   (forms (append primaries non-primaries)))
              
	      (dolist (form forms)
            
		(let ((root-path (first form)) (value-plist (rest form)))
		  (let ((self 
			 (with-error-handling (:timeout nil)
			   (the-object object (follow-root-path root-path)))))
		    (when self
		      (mapc #'(lambda(key expression) 
				(unless (member key keys-to-ignore)
				  (the-object self (set-slot! key (eval `(let ((self ,self)) ,expression)) :re-sort? nil))))
			    (plist-keys value-plist) (plist-values value-plist)))))))
	    object)))))


;;
;; FLAG -- remove this defunction version. 
;;
#+nil
(defun read-snapshot (&key (filename "/tmp/snap.gdl") object keep-bashed-values? make-object-args keys-to-ignore)
  "GDL Instance. Reads the snapshot file indicated by filename. If no optional keyword <tt>object</tt>
argument is given, a new GDL instance based on the data in the snapshot file is returned. If an
<tt>object</tt> is given, the object should be compatible in type to that specified in the 
snapshot file, and this existing object will be modified to contain the set slot values and
toplevel inputs as specified in the snapshot file.

:&key ((filename \"/tmp/snap.gdl\") \"String or pathname. File to be read.\"
       (keep-bashed-values? nil) \"Boolean. Indicates whether to keep the currently bashed values in object before reading snap values into it.\"
       (object nil) \"GDL object. Existing object to be modified with restored values.\")"
  (with-open-file (in filename)
    (let ((package-form (read in)))
      (when (or (null package-form) (not (find-package (second package-form))))
        (error "Invalid package specification at beginning of ~a.~%" filename))
      (let* ((*package* (find-package (second package-form))) (root-form (read in)))
        (when (or (null root-form) 
                  (not (eql (class-of (find-class (first root-form))) (find-class 'gdl-class))))
          (error "Invalid object type specifier as first element of second form in ~a.~%" root-form))

        
        (let ((object (cond ((and object keep-bashed-values?) object)
                            (object (the-object object restore-tree!) object)
                            (t (progn
                                 (apply #'make-object (first root-form) make-object-args))))))
          
          (let ((self object) (value-plist (rest root-form)))

            (mapc #'(lambda(key expression) 
                      (unless (member key keys-to-ignore)
                        (when self (the-object self (set-slot! key (eval expression))))))
                  (plist-keys value-plist) (plist-values value-plist)))
          
          (let ((forms-ht (make-hash-table)))
            (do ((form (read in nil nil) (read in nil nil)))
                ((null form))
              (push form (gethash (length (first form)) forms-ht)))
            (let* ((forms (list-hash forms-ht))
                   (forms (mapcar #'list (plist-keys forms) (plist-values forms)))
                   (forms (plist-values (alist2plist (sort forms #'< :key #'first))))
                   (forms (mapcar #'(lambda(group)
                                      (let ((primaries (remove-if-not #'(lambda(item) (or (getf (rest item) :%primary?%)
                                                                                          (getf (rest item) :element-index-list))) group))
                                            (non-primaries (remove-if #'(lambda(item) (or (getf (rest item) :%primary?%)
                                                                                          (getf (rest item) :element-index-list))) group)))
                                        (append primaries non-primaries))) forms))
                   (forms (apply #'append forms)))
              
              (dolist (form forms)
            
                (let ((root-path (first form)) (value-plist (rest form)))
                  (let ((self 
                         (with-error-handling (:timeout nil)
                           (the-object object (follow-root-path root-path)))))
                    (when self
                      (mapc #'(lambda(key expression) 
                                (unless (member key keys-to-ignore)
				  (the-object self (set-slot! key (eval `(let ((self ,self)) ,expression))))))
                            (plist-keys value-plist) (plist-values value-plist))))))))
          object)))))



(defun write-plist (&key plist output-path)
  "Pretty-prints a plist to a file with standard I/O syntax.

:&key ((plist nil) \"List. The list to be printed to the file\"
       (output-path nil) \"Pathname of a file. The file to be created or superseded.\")

"
  (with-open-file (out   output-path
                   :direction :output
                   :if-exists :new-version
                   :if-does-not-exist :create)
        (with-standard-io-syntax
          (pprint plist out))))
  



(defun min-max-search (function comparison low high &optional (tolerance *zero-epsilon*) verbose?)

  (when verbose? (print-variables high low))
  
  (if (near-to?  high low tolerance) high
    (let* ((mid (half (+ low high)))
           (lower (half (+ low mid)))
           (upper (half (+ mid high)))
           (lower-value (funcall function lower))
           (upper-value (funcall function upper)))
      
      (if (funcall comparison lower-value upper-value)
          (min-max-search function comparison low mid tolerance verbose?)
        (min-max-search function comparison mid high tolerance verbose?)))))


(defun binary-search (function value low high &key
						(tolerance *zero-epsilon*)
						(search-granularity *zero-epsilon*)
						verbose?)

  
  (when verbose? (print-variables high low))
  
  (if (near-to?  high low search-granularity)
      (progn (warn "binary search did not converge, returning best guess")
             high)
    (let* ((mid (half (+ low high)))
           (mid-value (funcall function mid)))
      
      (print-variables mid mid-value)
      
      (cond ((near-to? mid-value value tolerance) mid)
            ((> mid-value value) (binary-search function value low mid
						:tolerance tolerance
						:search-granularity search-granularity
						:verbose? verbose?))
            ((< mid-value value) (binary-search function value mid high
						:tolerance tolerance
						:search-granularity search-granularity
						:verbose? verbose?))))))

						



(defun gdl-rule::lookup-parameters (self message)
  (let ((value (getf (the %parameters%) message 'gdl-rule:%not-handled%)))
    (if (eql value 'gdl-rule:%not-handled%) (not-handled self message) value)))


(defparameter *dep-hash-threshhold* 1000)

(defparameter *invalid-aggregate-behavior* :error)


(defun add-notify-cons (notify-cons value &optional self message)
  ;;
  ;; FLAG -- this was added as a fix for github Issue #69, but causes
  ;; a regression by polluting the dependency graph with normal objects 
  ;; which may then become unbound spuriously. Retracted until a more
  ;; benign fix for Issue #69 can be determined. 
  ;;
  ;;(declare (ignore self message))
  ;;
  ;; FLAG -- we put back the extra tracking of notify-cons but only if
  ;; the message does not name a GDL object -- we don't want those
  ;; being the target of dependencies or they can be orphaned.
  ;;
  (when (and self message)
    (let ((aggregate  (gdl-acc::%aggregate% (first notify-cons))))
      (when (and (consp aggregate) (first aggregate)
		 (not (consp (gdl-acc::%aggregate% self))))
	(let ((agg (first aggregate)))
	  (if agg
	      (let ((num-value (gdl-acc::number-of-elements agg)))
		(when (and (consp num-value)
			   (let ((value (funcall message self)))
			     (not 
			      (and (consp value) 
				   (typep (first value) 'gdl-basis)))))
		  (add-notify-cons (list self message) num-value)))

	      (let ((error-message (format nil "
!!! 

 Invalid internal Gendl data structure detected. 

  notify-cons = ~s

  aggregate = ~a

  value = ~s

  self = ~a

  message = ~s


!!!
"
					   notify-cons aggregate value self message)))
		(ecase *invalid-aggregate-behavior*
		  (:error (error error-message))
		  (:warn (warn error-message))
		  (nil ))))))))
  (let ((second (second value)))
    (if (and (listp second) (< (length second) *dep-hash-threshhold*))
	(let ((matching-sublist (assoc (first notify-cons) (second value))))
	  (if matching-sublist (pushnew (second notify-cons) (rest matching-sublist))
	      (push (copy-list notify-cons) (second value))))
	(progn    
	  (when (listp second) (setf (second value) (alist-to-hash second)))
	  (pushnew (second notify-cons) (gethash (first notify-cons) (second value)))))))


(defun alist-to-hash (alist)
  (let ((ht (make-hash-table :size (twice *dep-hash-threshhold*) :rehash-size *dep-hash-threshhold*)))
    (dolist (entry alist ht)
      (destructuring-bind (object . messages) entry
	(setf (gethash object ht) messages)))))


#+nil
(defun add-notify-cons (notify-cons value &optional self message)

  (when (and self message)
    (let ((aggregate  (gdl-acc::%aggregate% (first notify-cons))))
      (when (consp aggregate)
	(add-notify-cons (list self message) 
			 (gdl-acc::number-of-elements (first aggregate))))))
  
  (let ((matching-sublist (assoc (first notify-cons) (second value))))
    (if matching-sublist (pushnew (second notify-cons) (rest matching-sublist))
	(push (copy-list notify-cons) (second value)))))


(defparameter *loaded-hotpatches* nil)

(defun load-hotpatches (&key (directory (merge-pathnames "hotpatches/" (glisp:executable-homedir-pathname))))

  (if (not (and (probe-file directory)
		(glisp:file-directory-p directory)))
      (warn "~s does not exist and/or is not a directory~%" directory)
      (let* ((files (directory directory))
	     (source-files (sort (remove-if-not #'(lambda(file) 
						    (let ((type (pathname-type file)))
						      (or (string-equal type "lisp")
							  (string-equal type "gdl")
							  (string-equal type "gendl")))) files)
				 #'string-lessp :key #'namestring)))
	(when (or (not (glisp:featurep :allegro))
		  (glisp:featurep :compiler))
	  (dolist (file source-files)
	    (with-error-handling ()
	      (#+allegro excl:compile-file-if-needed #-allegro compile-file file))))

	(setq files (directory directory))

	(let ((compiled-files (sort (let ((fasl-type (pathname-type (compile-file-pathname "foo"))))
				      (remove-if-not #'(lambda(file)
							 (string-equal (pathname-type file) fasl-type)) files))
				    #'string-lessp :key #'namestring)))
	  (dolist (file compiled-files)
	    (unless (find file *loaded-hotpatches* :test #'equalp)
	      (with-error-handling () 
		(load file)
		(pushnew file *loaded-hotpatches* :test #'equalp))))))))
    
    
      
    
			       
(defun double-length-sort (&optional (list))
  (let ((first (safe-sort list #'(lambda(item1 item2)
				   (let ((first1 (first (first item1)))
					 (first2 (first (first item2))))
				     (< (if (consp first1) (length first1) 0)
					(if (consp first2) (length first2) 0)))))))
    (stable-sort first #'(lambda(x  y) (< (length (first x)) (length (first y)))))))




