;;
;; Copyright 2002-2011 Genworks International 
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

(define-object skeleton-form-control (skeleton-ui-element)

  :documentation (:author "Dave Cooper, Genworks"
                  :description "Computes standard values for base-form-control and similar container objects, e.g. grid-form-control.

Does not perform the actual bashing and computation of result value, should be mixed in to something which does this.")

  :input-slots
  (("Boolean. Set this to t if the form-control should always occur first in an outputted snapshot file.
Defaults to nil."
    primary? nil)

   ("String. You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated."
    class nil))
  
  :computed-slots
  (
   (form-controls nil)

   
   
   ("String of valid HTML. Also known as simply form-control. 
This is the default HTML which can be included in a form in a web page to display this form control.
Default is the output from form-control method of the lens for html-format and the 
specific type of this object, returned as a string." 
    form-control-string (with-output-to-string (*stream*)
                          (with-format (html-format *stream*)
                            (write-the form-control))))
   
   
   ("String of valid HTML. This is the default HTML which can be included in a form in a web page to display this form control.
Previously known as form-control-string. Default is the form-control-string." 
    form-control (the form-control-string))
   
   
   ("String of valid HTML. This is the default HTML which can be included in a form in a web page to display this form control, wrapped with labels and table cells." 
    html-string (with-output-to-string (*stream*)
                  (with-format (html-format *stream*)
                    (write-the string))))
   
   ("Keyword symbol. The name of this field. Computed from the object name within the tree." 
    field-name (make-keyword-sensitive (the base64-encoded-root-path)))

                 
   
   ("Keyword symbol. The ID attribute for this tag. Defaults to (the field-name)." 
    id (the field-name))))



(define-object base-form-control (skeleton-form-control)
  
  :documentation (:author "Dave Cooper, Genworks"
                  :description "This object can be used to represent a single HTML form control. It captures the 
initial default value, some display information such as the label, and all the standard HTML tag attributes
for the tag e.g. INPUT, SELECT, TEXTAREA. GWL will process the data types according to specific rules,
and validate the typed value according to other default rules. A custom validation-function can also 
be provided by user code. 

Sequences of these objects (with :size, :indices, :matrix, and :radial) are supported.

This facility and its documentation is expected to undergo significant and frequent upgrades in the remainder of GDL 1573 and upcoming 1575.

Current to-do list:

<ol>

<li>
Currently this works with normal HTTP form submission and full page reloading. 
We intend to make it work with AJAX and surgical page update as well.

<li>
We intend to provide inputs for all the standard tag attributes for the accompanying LABEL tag for the form control.

<li>
Additional form control elements to be included, to cover all types of form elements specified in current HTML standard from

    http://www.w3.org/TR/html401/interact/forms.html#h-17.2.1

    <ul>
      <li>button-form-control: submit buttons, reset buttons, push buttons.
      <li>checkbox-form-control: checkboxes, radio buttons (multiple of these must be able to have same name)
      <li>menu-form-control: select, along with optgroup and option.
      <li>text-form-control: single-line text input (including masked passwords) and multi-line (TEXTAREA) text input.
      <li>file-form-control: file select for submittal with a form.
      <li>hidden-form-control: input of type hidden.
      <li>object-form-control: (not sure how this is supposed to work yet).
    </ul>
</ol>


Also, we have to study and clarify the issue of under what conditions values can possibly take on nil values, 
and what constitutes a required field as opposed to a non-validated field, and whether a blank string on a text
input should be represented as a nil value or as an empty string.

Note that checkbox-form-control and menu-form-control currently get automatically included in the possible-nils.


"

                  :examples "
<pre>

 (in-package :gwl-user)

 (define-object test-form (base-html-sheet)
  
   :objects
   ((username :type 'text-form-control
              :size 35
              :maxlength 30
              :allow-nil? t
              :default \"Ron Paul\")
   
    (age :type 'text-form-control
         :size 5
         :validation-function #'(lambda(input) (or (null input) (> 80 input 70)))
         :domain :number
         ;;:default 72
         :default nil )
   
    (bio :type 'text-form-control
         :rows 8
         :size 120
         :default \"
Congressman Ron Paul is the leading advocate for freedom in our nation's capital. 
As a member of the U.S. House of Representatives, Dr. Paul tirelessly works for 
limited constitutional government, low taxes, free markets, and a return to sound 
monetary policies. He is known among his congressional colleagues and his constituents 
for his consistent voting record. Dr. Paul never votes for legislation unless the 
proposed measure is expressly authorized by the Constitution. In the words of former 
Treasury Secretary William Simon, Dr. Paul is the one exception to the Gang of 535 on 
Capitol Hill.\")
   
    (issues :type 'menu-form-control
            :choice-list (list \"Taxes\" \"Health Care\" \"Foreign Policy\")
            :default \"Taxes\"
            :multiple? t)
   
    (color :type 'menu-form-control
           :size 7
           :choice-plist (list :red \"red\" 
                               :green \"green\" 
                               :blue \"blue\" 
                               :magenta \"magenta\" 
                               :cyan \"cyan\" 
                               :yellow \"yellow\" 
                               :orange \"orange\")
           :validation-function #'(lambda(color)
                                    (if (intersection (ensure-list color) 
                                                      (list :yellow :magenta))
                                        (list :error :disallowed-color-choice)
                                      t))
           ;;:append-error-string? nil
           :multiple? t
           :default :red
           ;;:onchange \"alert('hey now');\" 
           )
   
    (early-riser? :type 'checkbox-form-control
                  :default nil)
   
    (favorite-links :type 'text-form-control
                    :sequence (:size 3)
                    :size 70
                    :default \"http://\")))

 (define-lens (html-format test-form)()
   :output-functions
   ((main-sheet
     ()
     (with-html-output (*html-stream* nil :indent t)
       (:html (:head (:title \"Test Form\"))
              (:body (:h2 (:center \"Test Form\"))
                     (the write-development-links)
                     (with-html-form (:cl-who? t)
                       (:p (str (the username html-string)))
                       (:p \"(internal value is: \" (fmt \"~s\" (the username value)) \")\")
                       (:p (str (the age html-string)))
                       (:p \"(internal value is: \" (fmt \"~s\" (the age value)) \")\")
                       (:p (str (the bio html-string)))
                       (:p (:table 
                            (:tr (:td (str (the issues html-string))))
                            (:tr (:td (str (the color html-string))))))
                       (:p (str (the early-riser? html-string)))
                      
                       (dolist (link (list-elements (the favorite-links)))
                         (htm (str (the-object link html-string))))
                      
                       (:p ((:input :type :submit :value \" OK \"))))))))))
 
 (publish :path \"/fe\"
          :function #'(lambda(req ent)
                        (gwl-make-object req ent \"gwl-user::test-form\")))
</pre>
"
                  )
  
  
  :input-slots
  (("Lisp value of a type compatible with (the domain). This is the initial default value for the control. 
This must be specified by user code, or an error will result." 
    default (error "You must specify a default for the ~a at ~s." (the type) (reverse (the root-path))))
   
   ("String. The prompt used in the label. " prompt (string-capitalize 
                                                     (glisp:replace-regexp 
                                                      (string 
                                                       (if (null (the index)) 
                                                           (make-keyword (the %name%))
                                                         (make-keyword (format nil "~a#~a" (the %name%) (the index))))) "-" " ")))
   
   ("Boolean. Regardless of :domain, if this is non-nil, nil values will be accepted. Defaults to t if (the default) is nil, 
otherwise defaults to nil."
    allow-nil? (null (the default)))
   
   ("Boolean. If non-nil, then values which fail the type or validation test will still be allowed to be the value. Default is t."
    allow-invalid? t)
   
   ("Boolean. If non-nil, then values which fail the type test will still be allowed to be the value. Default is nil."
    allow-invalid-type? nil) 
   
   ("Boolean. Regardless of :domain, if this is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?)"
    nullify-empty-string? (the allow-nil?))
   
   
   ("Boolean. This switch determines whether this form-control should be preset before the final setting, 
in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil."
    preset? nil)
   
   ("Keyword symbol, one of :number, :keyword, :list-of-strings, :list-of-anything, or :string. 
This specifies the expected and acceptable type for the submitted form value. If possible, the 
submitted value will be coerced into the specified type. The default is based upon
 the Lisp type of (the default) provided as input to this object. If the default is nil,
the domain will default to :string" domain (if (null (the default))
					       :string
					       (typecase (the default)
						 (number :number)
						 (keyword :keyword)
						 (list (cond ((every #'stringp (the default))
							      :list-of-strings)
							     (t :list-of-anything)))
						 (otherwise :string))))
   
   ("Boolean. Determines whether a default error string is appended to string ouput-function for 
html-format (and therefore html-string computed-slot as well). Defaults to t."
    append-error-string? t)
   
   ("Keyword symbol or nil. Specifies where the label tag goes, if any. 
Can be :table-td (label goes in a td before the form control), :table-td-append (label goes in a td after the form control), 
:prepend (label tag wraps around form control and label text comes before form control),  
:append (label tag wraps around form control and label text comes after form control), 
:table-with-class (like :table-td, but adds a class \"form-control\" to the table), or
:as-div (puts label and control inside a div of class \"form-control\").

Default is :table-td"
    label-position :table-td)
   
   ("Function of one argument. The argument will be the submitted form value converted to the proper type. 
The return value from this function can be nil, any non-nil value, or a plist with keys :validated-value 
and :error. The following behavior applies:
 <ul>
 <li> If the function returns nil, error is set to  :unspecified-validation-fail.</li>
 <li> If the function returns a plist with keys :validated-value and :error, and if :error is non-nil, 
it means the value is not acceptable, the form-controls error message is set to this error (usually a keyword symbol),
and the error string will be appended to the html-string by default. </li>
 <li> If the function returns any other value, then the properly typed submitted form value is considered valid and is used. 
 </ul>

 In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value. If
 allow-invalid? is non-nil, then the form-control's value message is also set to this value (i.e. the invalid value is
 still accepted, even though a non-nil error is present).

 Default is (list :validated-value value :error nil)."
    validation-function #'(lambda(value) 
                            (declare (ignore value))
                            t
                            ;;(list :validated-value value :error nil)
                            ))
   
   (ajax-submit-on-change? nil)
   
   ("String. Text to place in the field by default, overwritten as soon as the field is selected. Works only in HTML5. Default is nil." placeholder nil)
   ("Boolean. Maps to HTML form control attribute of the same name. Default is nil." disabled? nil)
   ("Boolean. Maps to HTML form control attribute of the same name. Default is nil." readonly? nil)
   ("Boolean. Maps to HTML form control attribute of the same name. Default is nil." ismap?  nil)   
   ("Number or nil. Maps to HTML form control attribute of the same name. Default is nil." size  nil)
   ("Number or nil. Maps to HTML form control attribute of the same name. Default is nil." maxlength nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." src  nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." alt  nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." usemap nil)
   ("Integer or nil. Maps to HTML form control attribute of the same name. Default is nil." tabindex nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." accesskey nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onfocus nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onblur nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onselect nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." 
    onchange (when (the ajax-submit-on-change?) (the parent (gdl-ajax-call :form-controls (list self)))))
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onclick nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." ondblclick nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onmousedown nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onmouseup nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onmouseover nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onmousemove nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onmouseout nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onkeypress nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onkeydown nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." onkeyup nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." accept nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." lang nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." title nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." style nil)
   ("String or nil. Maps to HTML form control attribute of the same name. Default is nil." align nil))
   

  
  :computed-slots
  (("String or error object. This will be set to a validation error if any, 
and cleared when the error is gone." error nil :settable)
   
   ("Lisp value. The value which was attempted to be set but failed validation." 
    failed-value nil :settable)
   
   
   ("Lisp value. The current value of this form control." value (the default) :settable))


  :functions
  (("Void. Restores the default for the value, the failed-value, and the error."
    restore-defaults! () (the (restore-slot-defaults! (list :error :failed-value :value))))
   
   
   (validate-and-set!
    (raw-value)
    
    (let ((result-plist (the (validate-type raw-value))))
      
      (when (and (the allow-invalid-type?) (getf result-plist :error))
        (setq result-plist (list :typed-value raw-value :error nil)))
      
      (if (getf result-plist :error)
          (the (set-error-state! :value raw-value :error (getf result-plist :error)))
        (let ((value (getf result-plist :typed-value)))
          (setq result-plist (the (validate-value value)))
          (cond 
           ((null result-plist)
            (the (set-error-state! :value value :error :unspecified-validation-fail)))
           ((and (consp result-plist) (evenp (length result-plist)) 
                 (getf result-plist :error))
            (the (set-error-state! :value value
                                   :error (getf result-plist :error))))
           ((and (consp result-plist) (evenp (length result-plist)) 
                 (getf result-plist :validated-value))
            (the (set-new-value! (getf result-plist :validated-value))))
           (t (the (set-new-value! value))))))))
   
   (set-actual-slot! 
    (new-value)
    ;;(the (set-slot! :value new-value))
    (the (set-slot-if-needed! :value new-value :infer-types? nil)))

   
   (set-new-value!
    (new-value)
    (the (set-actual-slot! new-value))
    (the (restore-slot-defaults! (list :error :failed-value)))
    (list (the field-name) new-value))    
   
   (set-error-state!
    (&key value error)
    (when (the allow-invalid?) (the (set-actual-slot! value)))
    (the (set-slot! :error (format nil "~a" error)))
    (the (set-slot! :failed-value value)))

   
   (validate-value
    (typed-value)
    (funcall (the validation-function) typed-value))
   
   (tweak-raw-value-and-domain
    (&key raw-value domain)
    (values raw-value domain))
   
   
   (validate-type
    (raw-value)
    (let ((domain (the domain)))
      (multiple-value-bind (raw-value domain)
          (the (tweak-raw-value-and-domain :raw-value raw-value :domain domain))
        
        (cond 
         ((and (the allow-nil?) (the nullify-empty-string?) (equalp raw-value ""))
          (list :typed-value nil :error nil))
              
         (t (ecase domain
              
              (:pass-thru (list :typed-value (read-safe-string raw-value) :error nil))
              
              (:string (list :typed-value raw-value :error nil))
          
              (:boolean (let ((item (read-safe-string raw-value)))
                          (if item (list :typed-value t :error nil)
                            (list :typed-value nil :error nil))))
      
              (:keyword (multiple-value-bind (typed-value error)
                            (ignore-errors (make-keyword raw-value))
                          (list :typed-value typed-value 
                                :error (when (typep error 'error) error))))
        
              ((:list-of-strings :list-of-anything)
               (multiple-value-bind (typed-value error)
                   (ignore-errors (ensure-list (read-safe-string raw-value)))
                 (list :typed-value typed-value 
                       :error (when (typep error 'error) error))))
              
              (:number (multiple-value-bind (typed-value error)
                           (ignore-errors (read-safe-string raw-value))
                         (cond ((typep error 'error)
                                (list :typed-value nil 
                                      :error (when (typep error 'error) error)))
                               ((numberp typed-value)
                                (list :typed-value typed-value :error nil))
                               (t (list :typed-value typed-value 
                                        :error :should-be-a-number)))))
              
              (:symbol (multiple-value-bind (typed-value error)
                           (ignore-errors (if (keywordp raw-value) 
                                              raw-value
                                            (intern raw-value *package*)))
                         (list :typed-value typed-value 
                               :error (when (typep error 'error) error))))))))))))


(define-lens (html-format base-form-control)()
  :output-functions
  (
   (:label 
    (&key include-prompt?)
    (with-html-output (*stream* nil)
      ((:label :for (the field-name))
       (when include-prompt? (htm (str (the prompt)))))))
   
   (prompt
    ()
    (with-html-output (*stream* nil)
      (str (the prompt))))
   
   (form-control 
    ()
    (when *debug?* (format t "~&Writing from form-control of base-form-control~%"))
    (with-html-output (*stream*)
      "empty form-control (str (the root-path))"))
   
   (string
    ()
    (with-html-output (*stream* nil)
      (cond ((the prompt)
             (ecase (the label-position)
               (:table-td (htm (:table (:tr (:td (write-the (:label :include-prompt? t)))
                                            (:td (write-the form-control))))))
               (:table-td-append (htm (:table (:tr (:td (write-the form-control))
                                                   (:td (write-the (:label :include-prompt? t)))))))
               (:prepend (htm ((:label :for (the field-name)) (write-the prompt))
                              (write-the form-control)))
               (:append (htm (write-the form-control)
                             ((:label :for (the field-name)) (write-the prompt))))
               (:table-with-class (htm ((:table :class "form-control") 
                                        (:tr (:td (write-the (:label :include-prompt? t)))
                                             (:td (write-the form-control))))))
               (:as-div (htm ((:div :class "form-control")
                              (write-the (:label :include-prompt? t))
                              (write-the form-control))))))
            
            (t (write-the form-control)))
      (when (and (the error) (the append-error-string?))
        (htm :br (:i ((:span :style "color: red") 
                      (str (the error))
                      (when (the failed-value)
                        (htm ", failed value is: " (fmt "~s" (the failed-value))))))))))))


;; Text
;;
(define-object text-form-control (base-form-control)
  :documentation (:author "Dave Cooper, Genworks"
                  :description "This represents a INPUT TYPE=TEXT or TEXTAREA form control tag."
                  :examples "Please see base-form-control for all the examples.")
  
  :input-slots
  (("Integer. The number of rows. If more than 1, this will be a TEXTAREA. Defaults to 1." 
    rows 1)
   
   ("Boolean. Specifies whether this should be a password form control with obscured screen text. 
Note that this does not automatically give encrypted transmission to the server - you need SSL
for that. Defaults to nil. Use password-form-control to get a default of t." 
    password? nil)

   ("Boolean. Specifies whether this should be a number form control with support for numerical input. 
Defaults to nil. Use number-form-control to get a default of t." 
    number? nil)
   
   ("Integer. The number of columns for a TEXTAREA (if rows is > 1). Defaults to (the size)." 
    cols (the size)))

  :computed-slots
  ((str-ready-string (if (eql (the domain) :pass-thru)
                         (escape-string (format nil "~s" (the value)))
                       (escape-string (format nil "~a" (or (the value) ""))))))
  
  :functions
  ((set-actual-slot! 
    (new-value)
    (the (set-slot-if-needed! 
          :value  
          (if (and (stringp new-value)
                   (> (length new-value) 1)
                   (eql (aref new-value (1- (length new-value))) #\Newline))
              (subseq new-value 0 (1- (length new-value)))
            new-value)
          :infer-types? nil)))))

(define-object password-form-control (text-form-control)
  :computed-slots ((password? t)))


(define-object number-form-control (text-form-control)
  :input-slots ((min nil) (max nil) (step nil))
  :computed-slots ((number? t)))


(define-lens (html-format text-form-control)()
  :output-functions
  ((form-control
    ()
    
    (when *debug?* (print-messages value domain str-ready-string))
    
    (if (> (the rows) 1)
        (with-expanded-html-output (*stream* nil)
          ((:textarea :rows (the rows) :cols (the cols) :name (the field-name) :id (the field-name))
           (when (the str-ready-string) 
             (str (the str-ready-string)))))
      (with-expanded-html-output (*stream* nil)
        ((:input :type (cond ((the password?) :password)
			     ((the number?) :number)
			     (t :text) )
                 :value (when (the str-ready-string)
                          (the str-ready-string))

		 :min (when (and (the number?)
				 (the min))
			(the min))
		 :max (when (and (the number?)
				 (the max))
			(the max))
		 :step (when (and (the number?)
				 (the step))
			(the step))
                 :name (the field-name) :id (the field-name))))))))


;;
;; Select
;;
(define-object menu-form-control (base-form-control)
  :documentation (:author "Dave Cooper, Genworks"
                  :description "This represents a SELECT form control tag wrapping some OPTION tags.
OPTIONGROUP is not yet implemented, but will be."
                  
                  :examples "<pre>

 ...
 
 :objects
 ((menu-1 :type 'menu-form-control
          :choice-plist (list 1 \"one\" 2 \"two\")))

 ...

</pre>




Please see base-form-control for a broader example which uses more form-control primitives together.")

  
  :input-slots (("List. Display values, also used as return values, for selection list. Specify this or choice-plist, not both."
                 choice-list nil)
                
                ("Boolean. Are multiple selections allowed? Default is nil." multiple? nil)
                
		("List of keyword symbols. Each of these should match a key in the choice-plist, and where there is a 
match, that key will be disabled in the rendering."
		 disabled-keys nil)
		
                ("Boolean. Indicates whether this should be included in possible-nils. Defaults to (the multiple?)" 
                 possible-nil? (the multiple?))
                
                ("Plist. Keywords and display values for the selection list. Specify this or choice-list, not both."
                 choice-plist (if (null (the choice-list))
                                  (error "Either choice-list or choice-plist must be given.")
                                (mapcan #'(lambda(choice)
                                            (list choice choice)) (the choice-list))))
                
                ("Plist. Keywords and CSS style for display of each choice. The keys should correspond to the 
keys in choice-plist, or the items in choice-list if no choice-plist is given."
                 choice-styles nil)
                
                ("Number. How many choices to display" size 3)
                
                
                ("Predicate function of two arguments. Defaults based on type of first in choice-plist: 
eql for keywords, string-equal for strings, and equalp otherwise."
                 test (typecase (first (the effective-choice-plist))
                        (keyword #'eql)
                        (string #'string-equal)
                        (otherwise #'equalp))))

  :computed-slots ((effective-choice-plist (or (the choice-plist)
                                               (mapcan #'(lambda(choice)
                                                           (list choice choice)) (the choice-list))))
                   
                   (display-format-string (etypecase (second (the effective-choice-plist))
                                            (number "~s")
                                            (list "~s")
                                            (symbol "~s")
                                            (string "~a")))
                   
                   (format-string (ecase (the domain)
                                    ((:list-of-anything :keyword :number :symbol) "~s")
                                    ((:string :pass-thru) "~a"))))
  
  :functions
  ((tweak-raw-value-and-domain
    (&key raw-value domain)
    (if (the multiple?)
        (ecase domain
          (:keyword (multiple-value-bind (possible-list error)
                        (ignore-errors (read-safe-string raw-value))
                      (cond ((typep error 'error) (error error))
                            ((listp possible-list)
                             (values (mapcar #'make-keyword possible-list)
                                     :list-of-anything))
                            (t (values (make-keyword raw-value) :keyword)))))
          ((:string :pass-thru)
           (multiple-value-bind (possible-list error)
               (ignore-errors (read-safe-string raw-value))
             (cond ((typep error 'error) (error error))
                   ((listp possible-list)
                    (values possible-list :list-of-strings))
                   (t (values raw-value :string)))))
          
          ((:number :list-of-anything)
           (multiple-value-bind (possible-list error)
               (ignore-errors (read-safe-string raw-value))
             (cond ((typep error 'error) (error error))
                   ((listp possible-list)
                    (values (mapcar #'read-safe-string possible-list) :list-of-anything))
                   (t (values (read-safe-string raw-value) :number))))))
      (ecase domain
        (:list-of-anything (values (read-safe-string raw-value) :list-of-anything))
        (:symbol (values (read-safe-string raw-value) :symbol))
        (:keyword (values (make-keyword raw-value) :keyword))
        ((:string :pass-thru) (values raw-value :string))
        (:number (values raw-value :number)))))))
   

(define-lens (html-format menu-form-control)()
  :output-functions
  ((form-control
    ()
    (with-expanded-html-output (*stream* nil)
      ((:select :name (the field-name) :id (the field-name) 
        :multiple (when (the multiple?) t))
       (dolist (key (plist-keys (the effective-choice-plist)))

         (with-cl-who ()
           ((:option :value (format nil  (the format-string) key)
                     :style (getf (the choice-styles) key)
                     :multiple (the multiple?)
		     :disabled (when (member key (the disabled-keys)) "disabled")
                     :selected (let ((selected? (cond ((listp key)
						       (funcall (the test) key (the value)))
						      (t (member key (ensure-list (the value)) 
								 :test (the test))))))
				 (when selected? "selected")))
	    (fmt (the display-format-string)
                 (first (rest (member key (the effective-choice-plist) 
                                      :test (the test)))))))))))))
   

;;
;; Radio
;;

(define-object radio-form-control (menu-form-control)
  :input-slots
  (("Keyword symbol or nil. Specifies where the description for each radio goes, if any. 
Can be:

<dl>
<dt><strong>:paragraph-prepend (or :p-prepend or :p)</strong></dt>
<dd>Description goes in a paragraph tag before the input tag.</dd>
<dt><strong>:paragraph-append (or :p-append)</strong></dt>
<dd>Description goes in a paragraph tag after the input tag</dd>
<dt><strong>:table-row-prepend (or :table-tr or :table-tr-prepend)</strong></dt>
<dd>Description goes in a table cell wrapped in a table row before the input tag table cell</dd>
<dt><strong>:table-row-append (or :table-tr-append)</strong></dt>
<dd>Description goes in a table cell wrapped in a table row after the input tag table cell</dd>
<dt><strong>nil (or any other value)</strong></dt>
<dd>No description, only the bare input tag for the radio</dd>
</dl>

Default is :paragraph-append."
    description-position :paragraph-append)
   
   
   ("String. Allows you to specify a class for the table surrounding the radio input elements. Defaults to empty string."
    table-class ""))

  :computed-slots ((multiple? nil)))



(define-lens (html-format radio-form-control)()
  :output-functions
  (("Void. Outputs the actual input tag. Undocumented but in theory can be overridden in a subclass by user code.
Contact Genworks if you need this documented."
    input-tag
    (&key key count)
    (with-expanded-html-output (*stream* nil)
      ((:input :type :radio :name (the field-name) 
               :id (format nil "~a-~a" (the field-name) count)
	       :disabled (when (member key (the disabled-keys)) "disabled")
               :value (format nil (the format-string) key) 
               :checked (funcall (the test) key (the value))))))
   
   ("Void. Outputs the input tag with description. Undocumented but in theory can be overridden in a subclass by user code. 
Contact Genworks if you need this documented."
    labeled-input-tag 
    (&key key value count)
    (with-html-output (*stream*)
      (case (the description-position)
        ((:paragraph-prepend :p-prepend :p)
         (htm (:p (fmt (the display-format-string) value)
                  (write-the (input-tag :key key :count count)))))
        ((:paragraph-append :p-append)
         (htm (:p (write-the (input-tag :key key :count count))
                  (fmt (the display-format-string) value))))

        ((:table-row-prepend :table-tr :table-tr-prepend)
         (htm (:tr (:td (write-the (input-tag :key key :count count)))
                   (:td (fmt (the display-format-string) value)))))
        
        ((:table-row-append  :table-tr-append)
         (htm (:tr (:td (fmt (the display-format-string) value))
                   (:td (write-the (input-tag :key key :count count))))))
        
        (otherwise (write-the (input-tag :key key :count count))))))

   
   (form-control
    ()
    (with-html-output (*stream*)
      ((:table :class (the table-class))
       (let ((count -1))
         (mapc #'(lambda(key value) 
                   (write-the (labeled-input-tag :key key :value value 
                                                 :count (incf count))))
               (plist-keys (the effective-choice-plist))
               (plist-values (the effective-choice-plist)))))))))


;;
;; Button
;;

(define-object button-form-control (base-form-control)
  
  :input-slots 
  ((:label (the value))
   
   (content (the :label))
   
   (button-type :button)
   
   (default " OK ")))


(define-lens (html-format button-form-control)()
  :output-functions
  ((form-control
    ()
    (with-expanded-html-output (*stream* nil)
      ((:button :name (the field-name) 
                :id (the field-name)
                :type (the button-type)
                :value (the value))
       (str (the content)))))))




;;
;; Checkbox
;;

(define-object checkbox-form-control (base-form-control)

  :documentation (:author "Dave Cooper, Genworks"
                  :description "This represents a INPUT of TYPE CHECKBOX"
                  :examples "Please see base-form-control for all the examples.")
  
  :input-slots 
  (("Keyword symbol. The domain defaults to :boolean for the checkbox-form-control.
However, this can be overridden in user code if the checkbox is supposed to return 
a meaningful value other than nil or t (e.g. for a group of checkboxes with 
the same name, where each can return a different value)." domain :boolean)
   
   ("Boolean. Indicates whether this should be included in possible-nils. Defaults to t." 
    possible-nil? t)))





(define-lens (html-format checkbox-form-control)()
  :output-functions
  ((form-control
    ()
    (with-expanded-html-output (*stream* nil)
      ((:input :name (the field-name) 
               :id (the field-name)
               :type :checkbox
               :value :on
               :checked (if (the value) t)))))))




