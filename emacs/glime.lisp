;; $Id: //info.ravenbrook.com/project/gendl/master/genworks/emacs/gendl.lisp#5 $

(in-package :swank)

;;                            glime.lisp
;;           Nick Levine, Ravenbrook Limited, 2013-05-02
;;
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to implement GENDL customizations
;; for SLIME (collectively known as "Glime(TM)")
;;
;; The following features have been implemented so far:
;;
;; * modeline and completion help for the keywords accepted by
;;   gendl:define-object,
;;
;; * modeline and completion help for the keywords accepted inside an
;;   :objects or :hidden-objects argument to gendl:define-object,
;;   gendl:make-object, or gendl:make-self.
;;
;; The intended readership is project developers.
;;
;; This document is not confidential. See end for copyright
;; information.

(defparameter *gendl-slime-release* "0.1.1")


;; 2.  TESTS
;;
;; It's handy to be able to test this outside of the SLIME environmemnt.
;;
;; '("define-object" "empty" nil ":objects" ("" %cursor-marker%))
;; ->
;; "(define-object name mixin-list &key ===> objects <=== &allow-other-keys)"

(defun gendl-autodoc-test (raw-form)
  (let ((*buffer-package* (find-package "GDL-USER"))
        (*buffer-readtable* *readtable*))
    (car (autodoc raw-form))))


;; 3.  MACROS

;; We need a (very, very) basic and portable advice facility. The
;; potentially dangerous assumption it makes is that the original
;; function won't be redefined behind our backs. I can't think of an
;; easy way to prevent users from doing that. On the other hand,
;; there's no reason why they should.

;; The #+lispworks conditionalisations immediately below are present
;; purely as development aids.

#+lispworks
(dspec:define-dspec-alias def-wrap (name)
  `(defun ,name))

(defmacro def-wrap (name lambda-list &body body)
  "Wrap BODY around the function defined by NAME. Locally defined #'PUNT
   will access the original definition."
  (let* ((args-var (gensym "ARGS-"))
         (cache-form `(unless (get ',name 'gendl-unwrapped)
                        (setf (get ',name 'gendl-unwrapped) (symbol-function ',name))))
         (wrapped-body `(,@(loop for next = (first body)
                                 while (or (stringp next)
                                           (and (consp next)
                                                (eq (car next) 'declare)))
                                 collect (pop body))
                         (flet ((punt (&rest ,args-var)
                                  (apply (get ',name 'gendl-unwrapped) ,args-var)))
                           ,@body))))
    #+lispworks
    `(dspec:def (def-wrap ,name)
       ,cache-form
       (let ((lw:*redefinition-action* :quiet))
         (defun ,name ,lambda-list ,@wrapped-body))
       ',name)
    #-lispworks
    `(progn
       ,cache-form
       (setf (symbol-function ',name)
             (lambda ,lambda-list ,@wrapped-body)))))


(defun wrap-test-original (x)
  (cons :original x))

(defun test-def-wrap ()
  (def-wrap wrap-test-original (x)
    (cons :wrapped (cdr (punt (1+ x)))))
  (assert (equal (wrap-test-original 3) '(:wrapped . 4)))
  (def-wrap wrap-test-original (x)
    (punt x))
  (assert (equal (wrap-test-original 3) '(:original . 3))))

(test-def-wrap)

;; I keep needing these.

(defmacro orf (location form &environment env)
  (multiple-value-bind (vars values new setter getter)
      (get-setf-expansion location env)
    (when (cdr new)
      (error "Can't work with setf-expansion for ~s - ~d values from setter ~s"
             location (length new) new))
    (let ((current (car new)))
      `(let* (,@(mapcar 'list vars values)
              (,current ,getter))
         (or ,current
             (progn (setf ,current ,form)
               ,setter))))))

#-:lispworks
(#-clozure glisp:without-redefinition-warnings
	   #+clozure progn
 (defmacro when-let (binding &body body)
   (destructuring-bind (var val) binding
     `(let ((,var ,val))
        (when ,var
          ,@body)))))

#||
#-:lispworks
(glisp:without-redefinition-warnings
 (defmacro when-let* (bindings &body body)
   (if bindings
       `(when-let ,(car bindings)
          (when-let* ,(cdr bindings)
                     ,@body))
     `(progn ,@body))))
||#

;; We could do with more informative error handling than SLIME provides. Getting
;; this right can be quite a lot of work, but making a start is cheap.

(defmacro handling-whatever ((&rest options) &body body)
  (declare (ignore options))
  `(handler-bind ((serious-condition (lambda (condition)
                                       ;; Print the problem and decline to handle.
                                       (format *terminal-io* "~&;; Error: ~a~%" condition))))
     ,@body))


;; 4.  ADVISE SWANK FUNCTIONS
;;
;; The main defintions of the four functions wrapped below are all in
;; the contrib file swank-arglists.lisp.
;;
;; We use *autodoc-highlighting* to disable runaway hightlighting when
;; we're showing init-slots in the modeline. The method on
;; compute-enriched-decoded-arglist below might set *autodoc-highlighting*
;; to nil.

(defvar *autodoc-highlighting* t)

;; Used for modeline display of available arguments.
(def-wrap autodoc (raw-form &key print-right-margin)
  (let ((*autodoc-highlighting* *autodoc-highlighting*)
        ;; The default is to view symbols as if we were in the CL-USER
        ;; package. I can't see how this helps anybody, so I'm
        ;; disabling it.
        (*arglist-show-packages* nil))
    (punt raw-form :print-right-margin print-right-margin)))

(def-wrap decoded-arglist-to-string (arglist &key operator highlight print-right-margin)
  (if (message-list-arglist-p operator arglist)
      (print-message-list-for-autodoc arglist print-right-margin)
      (punt arglist :operator operator :highlight highlight)))

(def-wrap form-path-to-arglist-path (form-path form arglist)
  (when *autodoc-highlighting*
    (punt form-path form arglist)))

;; There are two different paths which call find-subform-with-arglist. We
;; want to know which one we're on and we do this by binding
;; *grovelling-for-arglist*.

(defvar *grovelling-for-arglist* nil)

;; Used for (keyword) completion.
(def-wrap find-immediately-containing-arglist (form)
  (let ((*grovelling-for-arglist* t))
    (punt form)))

;; This might be useful further down.
(defvar *form-with-arglist*)

;; Invoked by both autodoc and find-immediately-containing-arglist.
(def-wrap find-subform-with-arglist (form)
  (handling-whatever ()
    (let ((*form-with-arglist* form))
      (if (and *grovelling-for-arglist*
               (eq (car form) 'gendl:define-object))
          (find-define-object-subform form #'punt)
        (punt form)))))


;; 5.  KEYWORDS ACCEPTED BY DEFINE-OBJECT, MAKE-OBJECT, MAKE-SELF

;;; We make gendl:define-object appear to have the same arglist as
;;; gendl::%define-object% (modulo three keywords which Dave Cooper
;;; says aren't in use.)

(defparameter *define-object-unsupported*
  '(:methods :no-vanilla-mixin? :query-slots))

;; See (defgeneric extra-keywords) in swank-arglists.lisp
(defmethod extra-keywords ((operator (eql 'gendl:define-object)) &rest arguments)
  (values (reverse (let ((unsupported *define-object-unsupported*))
		     (loop for keyword-arg in (arglist.keyword-args (compute-enriched-decoded-arglist 'gendl::%define-object% arguments))
			unless (find (keyword-arg.keyword keyword-arg) unsupported)
			collect keyword-arg)))
          t nil))

(defmethod extra-keywords ((operator (eql 'gendl:make-object)) &rest args)
  (or (keyword-args-from-class-input-slots (car args))
      (call-next-method)))

(defmethod extra-keywords ((operator (eql 'gendl:make-self)) &rest args)
  (or (keyword-args-from-class-input-slots (car args))
      (call-next-method)))

(defun keyword-args-from-class-input-slots (classname-form)
  (when (and (consp classname-form)
             (= (length classname-form) 2)
             (eq (car classname-form) 'quote))
    (let ((classname (cadr classname-form)))
      (multiple-value-bind (proceed input-slots)
          (class-input-slots classname)
        (when proceed
          (loop for slot in input-slots collect
                (make-keyword-arg slot slot nil)))))))



;; 6.  HELP ON SPECIFIC DEFINE-OBJECT KEYWORDS
;;
;; We use the swank interface for "enriching" an arglist. The method
;; below determines whether we're working on a specific keyword

;; A call might look like this:
;; (compute-enriched-decoded-arglist define-object (empty nil :objects (#s(swank::arglist-dummy :string-representation ""))))
;; Note that SLIME has already chopped everything to the right of the
;; cursor maker; editing
;;     (define-object empty () :objects ( ) :documentation ())
;; with the cursor here:              -->^<--
;; is the same as editing (define-object empty () :objects ()).
;;
;; See (defgeneric compute-enriched-decoded-arglist) in swank-arglists.lisp
(defmethod compute-enriched-decoded-arglist ((operator (eql 'gendl:define-object)) arguments)
  (multiple-value-bind (decoded-arglist determining-args any-enrichment)
      (call-next-method)
    (destructuring-bind (&optional name mixins &rest spec-pairs)
        arguments
      (declare (ignore name mixins))
      ;; Has the user got as far as the keyword list yet?
      (when spec-pairs
        ;; Which keyword are they working on?
        (multiple-value-bind (key values)
            (active-keyword spec-pairs)
          (when key
            (when-let (this-keyword-arg (find key (arglist.keyword-args decoded-arglist)
                                              :key 'keyword-arg.keyword))
              ;; Mangle the arglist structure: only show the keyword we're working on.
              (let ((local-arglist (copy-arglist decoded-arglist)))
                (setf (arglist.keyword-args local-arglist) (list this-keyword-arg)
                      (arglist.rest local-arglist) nil
                      (arglist.allow-other-keys-p local-arglist) t
                      ;; Disable highlighting.
                      *autodoc-highlighting* nil)
                (return-from compute-enriched-decoded-arglist
                  ;; If we can, make the init-slots for this keyword appear to be
                  ;; its default initargs. Behold! That makes them look just right.
                  (values (augment-gendl-define-object (keyword-arg.keyword this-keyword-arg)
                                                       local-arglist
                                                       values)
                          determining-args t))))))))
    ;; &allow-other-keys and &rest don't contribute anything here.
    (setf (arglist.rest decoded-arglist) nil
	  (arglist.allow-other-keys-p decoded-arglist) nil)
    (values decoded-arglist determining-args any-enrichment)))

(defgeneric augment-gendl-define-object (keyword arglist values)
  (:documentation
   "We're editing the value for gendl:define-object keyword KEYWORD. Return
    an arglist based on ARGLIST which conveys more information about the
    VALUES being edited."
   )
  (:method (keyword arglist values)
   (declare (ignore keyword values))
   arglist))


;; 6.1. Keyword support for :objects & :hidden-objects

(defmethod augment-gendl-define-object ((keyword (eql :objects)) arglist values)
  (augment-gendl-define-object-for-objects keyword arglist values))

(defmethod augment-gendl-define-object ((keyword (eql :hidden-objects)) arglist values)
  (augment-gendl-define-object-for-objects keyword arglist values))

;; Perform basic checks and then treat this as an implicit make-object.
(defun augment-gendl-define-object-for-objects (keyword arglist values)
  (let ((keyword-arg (or (find keyword (arglist.keyword-args arglist) :key 'keyword-arg.keyword)
                         ;; Shouldn't get here; playing it safe.
                         (return-from augment-gendl-define-object-for-objects arglist)))
        (value (let ((value (car (last values))))
                 (if (consp value)
                     value
                   (return-from augment-gendl-define-object-for-objects arglist)))))
    (setf (keyword-arg.default-arg keyword-arg) (implicit-make-object value)))
  arglist)

;; Value is the object-form on which the user is working. It'll be a
;; list consisting of lisp objects interspersed with SWANK arglist-dummies.
(defun implicit-make-object (value)
  (or (multiple-value-bind (proceed name type input-slots)
          (destructure-implicit-make-object value)
        (when proceed
          (let ((safe-name (let ((*read-eval* nil))
                             (read-from-string (undummy name)))))
	    ;; Note use of intern, to ensure hardwired symbols arrive in *package*.
            `((,safe-name :type ,type &key ,@input-slots) &rest ,(cautious-intern "MORE-OBJECTS")))))
      ;; We haven't got far enough to say anything yet.
      `((,(cautious-intern "NAME") &key type &allow-other-keys) &rest ,(cautious-intern "MORE-OBJECTS"))))


;; 7.  KEYWORD COMPLETION

(defun find-define-object-subform (form finder)
  (destructuring-bind (&optional name mixins &rest spec-pairs)
      (cdr form)
    (declare (ignore name mixins))
    ;; Has the user got onto the keyword list yet?
    (when spec-pairs
      ;; Which keyword are they working on?
      (multiple-value-bind (key values)
          (active-keyword spec-pairs)
        (when key
          (let ((value (car (last values))))
            ;; Can we do anything with this keyword?
            (when-let (new-form ;; (To do: hive this off as a separate GF.)
                                (case key
                                  ((:objects
                                    :hidden-objects) (let ((keywords (loop for x in (cdr value) until (arglist-dummy-p x) collect x)))
                                                       (multiple-value-bind (proceed name type input-slots)
                                                           (destructure-implicit-make-object
                                                            (cons (intern (undummy (car value))) keywords))
                                                         (declare (ignore name input-slots))
                                                         (if proceed
                                                             `(gendl:make-self ,type)
                                                           ;; Nothing useful found. Bail out.
                                                           (return-from find-define-object-subform
                                                             (values '(gendl:make-self)
                                                                     (make-arglist :key-p t
                                                                                   :keyword-args (unless (find :type keywords)
                                                                                                   (list (make-keyword-arg :type :type nil)))
                                                                                   :allow-other-keys-p t)))))))))
              ;; Apply original defintion of find-subform-with-arglist to gendl:make-self form.
              (return-from find-define-object-subform
                (funcall finder new-form))))))))
  ;; No? Apply original defintion of find-subform-with-arglist to original
  (funcall finder form))



;; 8.  DESTRUCTURING IMPLICIT MAKE-OBJECT FORMS

(defun destructure-implicit-make-object (form)
  "If FORM destructures as (SLOTNAME (quote CLASSNAME) ...) and CLASSNAME
   names a subclass of gendl:vanilla-mixin* then return four values: t,
   SLOTNAME, (quote CLASSNAME), and the class's init-slots. Otherwise
   return nil."
  (multiple-value-bind (name classname-form)
      (ignore-errors                                   ; catch destructuring-bind errors
        (destructuring-bind (name &key ((:type classname-form)) &allow-other-keys)
            (if (oddp (length form))                   ; is (cdr form) currently a well-formed plist?
                form
              (butlast form))
          (values name classname-form)))
    ;; CLASSNAME-FORM destructures as 'CLASSNAME.
    (let ((classname (when (and (listp classname-form)
                                (eq (car classname-form) 'quote))
                       (cadr classname-form))))
      (multiple-value-bind (proceed input-slots)
          (class-input-slots classname)
        (values proceed name classname-form input-slots)))))

(defun class-input-slots (classname)
  "Use message-list on a prototype instance of CLASSNAME to determine its
   required and optional input-slots. Return two values: t and the slots.
   If the class wasn't a subclass of gendl:vanilla-mixin* then return nil"
  (when (and classname
             (symbolp classname)
             (when-let (class (find-class classname nil))
               (subtypep class 'gendl:vanilla-mixin*)))
    (let ((prototype (gendl-class-prototype classname)))
      (values t
              (append (gendl:the-object prototype (message-list :category :required-input-slots))
                      (remove-if 'internal-name-p
                                 (gendl:the-object prototype (message-list :category :optional-input-slots))))))))

(defun gendl-class-prototype (class)
  "We could access the lisp implementation's CLOS class-prototype, but
can't expect that to have any boundp slots. For some of our uses that
doesn't matter. For others it does. So we'll make an object and cache it."
  (let ((class-name (etypecase class
                      (symbol class)
                      (class (class-name class)))))
    (orf (get class-name 'gendl-class-prototype)
         (gendl:make-object class))))

(defun internal-name-p (keyword)
  (let ((symbol-name (symbol-name keyword)))
    (and (char= (schar symbol-name 0) #\%)
         (char= (schar symbol-name (1- (length symbol-name))) #\%))))


;; 9.  HELP ON MESSAGES FOR GENDL:THE AND FRIENDS

;; We're only looking at the top of the reference-chain. See
;; http://paste.lisp.org/display/137274
;; Note that (the (slot-source :bar)) gives you info about the
;; specified :type of bar.  "You also have to establish the fact that
;; bar is indeed a GDL object and not some leaf-level value as with a
;; computed-slot (like a string or number). That can be done with
;; :return-category t."

(defparameter *message-locators*
  '(messages-in-this-form
    messages-from-classes
    messages-for-evaluate)
  "A list of functions, each taking a (gendl:define-object ...) form,
each returning a list of proposed messages.")

(defmethod compute-enriched-decoded-arglist ((operator (eql 'gendl:the)) arguments)
  (declare (ignorable arguments))
  (or (embedded-the-arglist) (call-next-method)))

(defmethod compute-enriched-decoded-arglist ((operator (eql 'gendl:the-object)) arguments)
  (declare (ignorable arguments))
  (or (embedded-the-arglist) (call-next-method)))

(defmethod compute-enriched-decoded-arglist ((operator (eql 'gendl:the-child)) arguments)
  (declare (ignorable arguments))
  (or (embedded-the-arglist) (call-next-method)))

(defun compute-this-the-messages (this-the whole-form)
  (declare (special gendl:self))
  (let ((gendl:self (or (when-let (self-form (this-the-self-form this-the))
			  (eval-form-to-object self-form))
			gendl:self)))
    (declare (special gendl:self))
    (remove-duplicates
     (loop for locator in *message-locators* append
	  (funcall locator this-the whole-form))
     :from-end t)))

(defun embedded-the-arglist ()
  ;;DJC
  ;;
  ;; Fixes "error in process filter" from within getpid of watchdog/source/assembly.lisp.
  ;;
  (ignore-errors
    (let* ((whole-form *form-with-arglist*)
	   (this-the (or (this-the-from-form whole-form)
			 ;; The gendl:the form isn't embedded inside a define-object or
			 ;; one of the gendl:the macros. Bail out.
			 (return-from embedded-the-arglist
			   nil))))
      (when-let (messages (compute-this-the-messages this-the whole-form))
	(values (or (loop for message in messages
		       when (arglist-p message)
		       return message)
		    (make-arglist :key-p t
				  :keyword-args (loop for message in messages collect
						     (make-keyword-arg message message nil))
				  :provided-args nil
				  :allow-other-keys-p t
				  :rest 'reference-chain))
                nil t)))))


;; 9.1. Analyse the current gendl:the form

(defstruct (this-the
	     (:constructor make-this-the (operator the-form &key section slot-form self-form 
                                                  &aux
                                                  (functionp (when (consp the-form)
                                                               (the-form-functionp (cdr the-form))))
                                                  (quantifiedp (quantified-objects-p section slot-form the-form)))))
  operator
  the-form
  section
  slot-form
  self-form
  functionp
  quantifiedp)

;; Select "(the ... (..."
(defun the-form-functionp (form)
  (when form
    (or (and (or (eq (cadr form) '%cursor-marker%)
                 (null (cdr form)))
             (let ((operator (car form)))
               (when (consp operator)
                 (let ((what (car operator)))
                   (if (or (eq what 'gendl:evaluate)
                           (and (consp what)
                                (eq (car what) 'gendl:evaluate)))
                       :evaluate
                     t)))))
        (the-form-functionp (cdr form)))))

;; If we are inside an :object or :hidden-object specification, and
;; one of the input keys is ":sequence", then "(the-child ..."  should
;; include display of the sequence messages as well as (in this case)
;; the user-visible messages for the child class.  Otherwise, the
;; "sequence messages" should be suppressed.
;;
;; http://paste.lisp.org/display/137258

(defun quantified-objects-p (section slot-form the-form)
  (case section
    ((objects hidden-objects) 
     (destructuring-bind (operator &rest more)
         the-form
       (declare (ignore more))
       (case operator
         ((gendl:the-child)
          ;; It's a the-child inside an :object or :hidden-object.
          (not (null (find-key :sequence (cdr slot-form))))))))
    ;; *form-with-arglist* is (gendl:the ...). Is it also (gendl:the (...) ...)?
    ((nil)
     (consp (car the-form)))))

;; Try repeating the name of this function, many times, fast, and in a darkened room.

(defun this-the-from-form (form)
  ;; A bit like messages-in-this-form below, best done as a separate cycle.
  (let ((operator (car form)))
    (case operator
      ((gendl:define-object)
       (destructuring-bind (classname &optional mixins
                                      &key input-slots computed-slots objects hidden-objects functions methods
                                      &allow-other-keys)
           (cdr form)
         (declare (ignore classname mixins))
         (loop for section in (list input-slots computed-slots objects hidden-objects functions methods)
               as section-name in '(input-slots computed-slots objects hidden-objects functions methods)
               do
               (loop for item in section
                     when (consp item)
                     do
                     (when-let (form-with-cursor-marker (form-with-cursor-marker item))
                       (return-from this-the-from-form
                         (make-this-the operator form-with-cursor-marker :section section-name :slot-form item)))))))
      ((gendl:the)
       (when-let (form-with-cursor-marker (cdr form))
         (make-this-the operator form-with-cursor-marker)))
      ((gendl:the-object)
       (when-let (form-with-cursor-marker (cddr form))
	 (make-this-the operator form-with-cursor-marker :self-form (cadr form)))))))

(defun form-with-cursor-marker (form)
  ;; Return, if any, the innermost (the ...) form containing the
  ;; cursor-marker.
  (loop for thing in form
        do
        (cond ((eq thing '%cursor-marker%) (return form))
              ((consp thing)
               (when-let (inner-form (form-with-cursor-marker thing))
                 (if (and (consp inner-form)
                          (case (car inner-form)
                            ((gendl:the gendl:the-child gendl:the-object) t)))
                     (return inner-form)
                   (return thing)))))))


;; 9.2. Messages which we can deduce from the current form.

(defun messages-in-this-form (this-the form)
  "Do what define-object itself does, to figure out the messages being
defined by this form."
  (let ((operator (this-the-operator this-the))
        (the-form (this-the-the-form this-the)))
    (unless (and (eq operator 'gendl:define-object)
                 (eq (car the-form) 'gendl:the)                            ; exclude gendl:the-object and gendl:the-child
                 (let ((the-what (cadr the-form)))
                   (or (eq the-what '%cursor-marker%)                      ; exclude past the head on reference-chains
                       (and (arglist-dummy-p the-what)
                            (string= (arglist-dummy.string-representation the-what) ""))))
                 ;; other exclusions?
                 )
      (return-from messages-in-this-form
        nil)))
  (destructuring-bind (classname &optional mixins
                                 &key input-slots computed-slots objects hidden-objects functions methods
                                 &allow-other-keys)
      (cdr form)
    (declare (ignore classname mixins))
    ;; [logic lifted from gendl::with-gdl-message-symbols]
    (let ((keyword (find-package :keyword)))
      (loop for section in (list input-slots computed-slots objects hidden-objects functions methods)
            append
            (with-buffer-syntax ()
              (loop for item in section
                    for symbol = (cond ((symbolp item) item)
                                       ((consp item) (unless (form-with-cursor-marker item)
                                                       ;; Exclude the message we're in the process of creating.
                                                       (let ((first (gendl::first-symbol item)))
                                                         (typecase first
                                                           (symbol first)
                                                           (arglist-dummy (from-string (arglist-dummy.string-representation first))))))))
                    when symbol
                    collect
                    (if (eq (symbol-package symbol) keyword)
                        symbol
                      (intern (symbol-name symbol) keyword))))))))


;; 9.3. Messages which we can deduce from mixin classes.

(defparameter *messages-to-suppress*
  (list ;; from vanilla-mixin*:
        :aggregate :all-mixins :children-uncached :direct-mixins :documentation
	:hidden? :leaves-uncached :message-documentation :message-list :mixins :name-for-display
	:restore-all-defaults! :restore-tree! :root :root-path-local :root? :safe-children
	:safe-hidden-children :slot-documentation :slot-source :slot-status :update! :visible-children
	:write-snapshot
        ;; from base-object:
        :image-file :local-bbox :local-box :local-center :local-center*
        :local-orientation :obliqueness
        ))

(defparameter *messages-to-suppress-when-not-sequence-member*
  (list :first? :index :last? :next :previous))

(defun gendl-source (class slot-sym)
  (cadr (gendl:the-object (gendl-class-prototype class) (slot-source (glisp:intern slot-sym :keyword)))))

(defun messages-from-classes (this-the form)
  "Use message-list to find out what the messages might be."
  (unless (eq (this-the-functionp this-the) :evaluate)
    (let* ((starting-classes (or (starting-classes this-the form)
                                 (return-from messages-from-classes
                                   nil)))
           (mixins (remove-duplicates (append (mapcar 'class-name starting-classes)
                                              (loop for class in starting-classes append
                                                    (gendl:the-object (gendl-class-prototype class) all-mixins)))
                                      :from-end t))
           (messages (remove-duplicates (loop for class in mixins
                                              append (sort (messages-from-class this-the class)
                                                           'string<)))))
      (filter-sequence-messages this-the messages))))

(defun starting-classes (this-the form)
  (case (car form)
    ((gendl:define-object)
     (destructuring-bind (classname &optional mixins &rest keywords)
         (cdr form)
       (declare (ignore keywords))
       (let ((this-class (or (the-child-class this-the)
                             (safe-find-class classname))))
         (if this-class
             ;; It's a either a (the-child ...) class , or a redefinition
             ;; (which includes: clicking on an existing defintion).
             (the-reference-chain this-the this-class)
           ;; New definition. Hit the superclasses.
           (loop for classname in mixins
                 for class = (safe-find-class classname)
                 when class collect class)))))
    ((gendl:the gendl:the-object)
     (locally (declare (special gendl:self))
       (when (boundp 'gendl:self)
         (when-let (self gendl:self)
           (let ((reference-chain (the-reference-chain this-the (class-of self))))
             (list (car reference-chain)))))))))

;; If we're in a the-child inside an :objects or :hidden-objects clause and can find
;; a class for the :type, return that.
(defun the-child-class (this-the)
  (when (and (case (this-the-section this-the)
               ((objects hidden-objects) t))
             (eq (car (this-the-the-form this-the)) 'gendl:the-child))
    (object-slot-form-class (cdr (this-the-slot-form this-the)))))

(defun object-slot-form-class (slot-form)
  (multiple-value-bind (typep quoted-type)
      (find-key :type slot-form)
    (declare (ignore typep))
    (when (and (consp quoted-type)
               (eq (car quoted-type) 'quote)
               (symbolp (cadr quoted-type)))
      (find-class (cadr quoted-type) nil))))

(defun the-reference-chain (this-the class)
  (unless (arglist-dummy-p class)
    (let* ((the-form (this-the-the-form this-the))
           (reference-chain (case (this-the-operator this-the)
                              ((gendl:define-object)
                               (case (car the-form)
                                 ((gendl:the gendl:the-child) (cdr the-form))
                                 ((gendl:the-object)          (cddr the-form))))
                              ((gendl:the gendl:the-child gendl:the-object) the-form))))
      (loop for reference in reference-chain do
            (setf class (or (reference-class reference class)
                            (return)))))
    (list class)))

(defun reference-class (reference class)
  (if (or (arglist-dummy-p reference)
          (consp reference)                         ; function call
          (eq reference '%cursor-marker%))
      class
      (object-slot-form-class (gendl-source class reference))))

;; [gendl] make your own defparameter for now, but flag it that it
;; might be redundant with something we already have defined
(defparameter *internal-packages*
  (loop for designator in
        '(:gendl :geom-base :surf :gwl :yadd :tasty)
        collect
        (find-package designator)))

(defun messages-from-class (this-the class)
  "Return a list of messages. Take anything from a user-defined class (not
in one of the *internal-packages*), otherwise filter for non-nil message-remarks."
  (unless (find class '(gendl::gdl-basis standard-object t))
    (multiple-value-bind (functionp current)
        (when this-the
          (values (this-the-functionp this-the)
                  ;; Use this to check we don't return the current slot as a potential message.
                  ;; But messages -- as in (the message-list) -- are keywords and this isn't...
                  (when-let (current (car (this-the-slot-form this-the)))
                    (if (arglist-dummy-p current)
                        (intern (arglist-dummy.string-representation current) :keyword)
                      (glisp:intern current :keyword)))))
      (let* ((prototype (gendl-class-prototype class))
             (all-messages (gendl:the-object prototype
                                             (message-list :message-type :local
                                                           :filter (if (find (symbol-package class)
                                                                             *internal-packages*)
                                                                       (lambda (category keyword)
                                                                         (declare (ignore category))
                                                                         (not (or (eq keyword current)
                                                                                  (find keyword *messages-to-suppress*)
                                                                                  (null (gendl:the-object prototype (message-remarks keyword))))))
                                                                     (lambda (category keyword)
                                                                       (declare (ignore category))
                                                                       (not (eq keyword current)))))))
             (sequences (append (gendl:the-object prototype (message-list :category :quantified-objects
                                                                          :message-type :local))
                                (gendl:the-object prototype (message-list :category :quantified-hidden-objects
                                                                          :message-type :local))))
             (functions (gendl:the-object prototype (message-list :category :functions
                                                                  :message-type :local))))
        ;; Preserve order
        (loop for message in all-messages
              when (if (and sequences
                            (find message sequences))
                       (this-the-quantifiedp this-the)
		       (if functionp
			   (find message functions)
			   (or (not (find message functions))
			       ;; include functions with no required args
			       (not (required-args-p (car (gendl-source class message)))))))
              collect message)))))

(defun filter-sequence-messages (this-the messages)
  (if (this-the-quantifiedp this-the)
      messages
    ;; Again, we preserve order
    (let ((suppress *messages-to-suppress-when-not-sequence-member*))
      (loop for message in messages
            unless (find message suppress)
            collect message))))


;; 9.4. Deal with (the ... (evaluate ...))
 
(defun messages-for-evaluate (this-the form)
  (declare (ignore form))
  (when (eq (this-the-functionp this-the) :evaluate)
    (list (make-arglist :required-args '(expression)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar +message-list-operators+ '(gendl:the gendl:the-object gendl:the-child))

(defun message-list-arglist-p (operator arglist)
  (and (member operator +message-list-operators+)
       (arglist.key-p arglist)
       (eql (+ (length (arglist.required-args arglist)) (length (arglist.optional-args arglist)))
	    (length (arglist.provided-args arglist)))))

(defun print-message-list-for-autodoc (arglist print-right-margin)
  (with-output-to-string (*standard-output*)
    (with-arglist-io-syntax
      (let ((*print-right-margin* print-right-margin))
	(pprint-logical-block (nil nil)
	  (loop for first = t then nil
	     for keyarg in (arglist.keyword-args arglist)
	     do (unless first
		  (write-char #\space)
		  (pprint-newline :fill))
	     do (princ (keyword-arg.keyword keyarg))))))))


;; 10.  UTILITIES

(defun cautious-intern (string)
  (if (eq (readtable-case *readtable*) :preserve)
      ;; Allegro modern mode?
      (intern (string-downcase string))
    (intern string)))

(defmethod safe-find-class ((self symbol)) (find-class self nil))
(defmethod safe-find-class ((self class))  self)
(defmethod safe-find-class ((self t))      nil)

;; Look graciously for key: don't assume list is correctly formed for destructuring-bind.
(defun find-key (key plist)
  (when (consp plist)
    (loop for (k v) on plist by #'cddr
          when (eq k key)
          do (return (values k v)))))

;; List is of form (:keyword stuff :keyword stuff ...); the final stuff may be missing.
;; Return the final (:keyword stuff) pair as two values, provided stuff is a list.
;; Otherwise return nil.
(defun active-keyword (list)
  (let ((key nil)
        (stuff nil))
    (loop (setf key (pop list))
          (if list
              (setf stuff (pop list))
            (return-from active-keyword
              nil))
          (unless list
            (return)))
    (when (listp stuff)
      (values key stuff))))

(defun test-active-keyword ()
  (loop for (list . result) in '((() . nil)
                                 ((:foo) . nil)
                                 ((:foo bar) . nil)
                                 ((:foo (bar)) . (:foo (bar)))
                                 ((:foo (bar) :baz) . nil)
                                 ((:foo (bar) :baz ()) . (:baz nil))
                                 )
        do
        (multiple-value-bind (keyword stuff)
            (active-keyword list)
          (if keyword
              (assert (equal (list keyword stuff) result))
            (null result)))))

(test-active-keyword)

(defun required-args-p (lambda-list)
  (loop while lambda-list
     for arg = (pop lambda-list)
     do (if (memq arg '(&whole &environment))
	    (pop lambda-list) ;; ignore
	    (return (not (or (memq arg lambda-list-keywords)
			     (and (symbolp arg) (string= (symbol-name arg) (string '#:&any)))))))))

(defvar *eval-the-object-form* t
  "If true, attempt to evaluate the first argument to THE-OBJECT when looking for messages.  This opens up
a security hole but is mighty convenient.")

(defun eval-form-to-object (form)
  (let ((object (ignore-errors (if *eval-the-object-form*
				   (eval form)
				   (labels ((eval-form (form)
					      (cond ((symbolp form)
						     (multiple-value-bind (exp win) (macroexpand form)
						       (if win
							   (eval-form exp)
							   (symbol-value form))))
						    ((atom form) form)
						    (t (error "Form too complex: ~s" form)))))
				     (eval-form form))))))
    (and (typep object 'gendl:vanilla-mixin*) object)))


;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2013-05-02 NDL Created.
;;
;; 2013-05-09 NDL Version 0.1 as http://paste.lisp.org/display/137088
;;
;; 2013-05-10 NDL Added *gendl-slime-release*.
;;                Release 0.1.1 as http://paste.lisp.org/display/137101
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright 2013 Genworks International
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
