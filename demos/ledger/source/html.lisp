;;
;; Copyright 2002, 2009 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language (GDL) project (GDL).
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

(in-package :ledger)

(define-object html-assembly (base-ajax-sheet)
  
  :objects
  ((ledger :type 'assembly)
  
   (view-accounts :type 'view-accounts
		  :account-sequence (the ledger accounts))
   
   (view-transactions :type 'view-transactions
		      :transaction-sequence (the ledger transactions)
		      :accounts-sequence (the ledger accounts))
   
   (add-transaction :type 'add-transaction
		    :main-sheet self
		    :accounts-sequence (the ledger accounts))
   
   (add-account :type 'add-account
		:main-sheet self))

		
  :functions
  (
   (write-html-sheet
    ()

    (let ((plist (the add-transaction query-plist)))
      (when plist
	(the ledger 
	  (add-transaction! 
	   :from-account (read-safe-string (getf plist :from-account))
	   :to-account (read-safe-string (getf plist :to-account))
	   :date (iso-to-universal (getf plist :date))
	   :amount (read-safe-string (getf plist :amount))
	   :payee (getf plist :payee))))
      (the add-transaction (:set-slot! :query-plist nil)))

    (let ((plist (the add-account query-plist)))
      (when plist
	(the ledger (add-account! 
		     :name (getf plist :name)
		     :description (getf plist :description)
		     :account-number (getf plist :account-number)
		     :account-type (getf plist :account-type)
		     :account-class (getf plist :account-class)
		     :beginning-balance (read-safe-string 
					 (getf plist :beginning-balance)))))
      (the add-account (:set-slot! :query-plist nil)))
    
    (html 
      (:html 
	(:head (:title "Personal Ledger"))
	(:body 
	 (:h2 (:center "Personal Ledger"))
       
	 (:p (:table 
		 (:tr (:td "Net Worth:")
		      ((:td :align :right) 
		       (:b (:tt ((:font 
				  :color (gethash (if (minusp (the ledger net-worth)) 
						      :red :green-lime) *color-table*))
				 (:princ (number-format (the ledger net-worth) 2)))))))
	       (:tr (:td "Profit/Loss:")
		    ((:td :align :right)
		     (:b (:tt ((:font 
				:color (gethash (if (minusp (the ledger profit)) 
						    :red :green-lime) *color-table*))
			       (:princ (number-format (the ledger profit) 2)))))))))
	 (:p (:ul (:li (the view-accounts 
			    (write-self-link :display-string "View Accounts")))
		  (:li (the view-transactions 
			    (write-self-link :display-string "View Transactions")))
		  (:li (the add-transaction 
			    (write-self-link :display-string "Add Transaction")))
		  (:li (the add-account 
			    (write-self-link :display-string "Add Account")))))))))))



(defun adaptive-lessp (item1 item2)
  (if (and (numberp item1)
	   (numberp item2))
      (< item1 item2)
      (string-lessp (format nil "~a" item1)
		    (format nil "~a" item2))))

(defun adaptive-greaterp (item1 item2)
  (if (and (numberp item1)
	   (numberp item2))
      (> item1 item2)
      (string-greaterp (format nil "~a" item1)
		       (format nil "~a" item2))))


(define-object view-accounts (base-ajax-sheet)
  :input-slots (account-sequence)
  
  :computed-slots
  ((accounts (list-elements (the account-sequence)))

   (accounts-sorted 
    (if (the sort-key)
	(safe-sort (the accounts)
		   (ecase (the sort-order) 
		     (:ascending #'adaptive-lessp)
		     (:descending #'adaptive-greaterp))
		   :key #'(lambda(account)
			    (the-object account (evaluate (the sort-key)))))
	(the accounts)))

   (sort-key nil :settable)
   (sort-order :ascending :settable)
   
   (headings (the-object (first (the accounts)) headings))

   (main-sheet-body (with-cl-who-string ()
		      (when gwl:*developing?* (str (the development-links)))
		      (:h2 (:center "Account Listing"))
		      (:p (the (:write-back-link :display-string "Home")))
		      (:p (str (the main-section main-div)))
		      (:p (the (:write-back-link :display-string "Home"))))))
  
  :functions ((set-sort-key! 
	       (&key sort-key)
	       (if (eql sort-key (the sort-key))
		   (the (set-slot! :sort-order (ecase (the sort-order)
						 (:ascending :descending)
						 (:descending :ascending))))
		   (the (set-slot! :sort-key sort-key)))))

  :objects
  ((main-section 
    :type 'sheet-section
    :inner-html 
    (with-cl-who-string ()
      
      (:p ((:table :bgcolor :black)
	   ((:tr :bgcolor :yellow) 
	    (dolist (heading (append (rest (the headings)) 
				     (list "Current Balance")))
	      (let ((ajax-call (the (gdl-ajax-call 
				     :function-key :set-sort-key!
				     :arguments (list :sort-key
						      (make-keyword 
						       (format nil "~(~a~)"
							       (glisp:replace-regexp
								heading " " "-"))))))))
	      (htm (:th ((:span :onclick ajax-call
				:style "color: blue; cursor: pointer;")
			 (str heading)))))))
	   (dolist (account (the accounts-sorted))
	     (htm 
	       ((:tr :bgcolor (gethash :grey-light-very *color-table*))
		(dolist (slot (list :name :description :account-number :account-type 
				    :account-class :beginning-balance :current-balance))
		  (let* ((raw-value (the-object account (evaluate slot)))
			 (value (case slot 
				  ((:beginning-balance :current-balance)
				   (number-format raw-value 2))
				  (otherwise raw-value))))
		    (htm ((:td :align (case slot 
					 ((:beginning-balance :current-balance)
					  :right)
					 (otherwise :left))) 
			   (case slot ((:beginning-balance :current-balance)
				       (htm (:tt (fmt "$~$" value))))
				 (otherwise (htm (str value)))))))))))))))))

			   
			     
(define-object view-transactions (base-ajax-sheet)

  :input-slots (transaction-sequence accounts-sequence)

  :computed-slots ((transactions (list-elements (the transaction-sequence)))

		   (headings (the-object (first (the transactions)) headings))

		   (main-sheet-body (with-cl-who-string ()
				      (:h2 (:center "Transaction Listing"))
				      (:p (the (:write-back-link :display-string "Home")))
				      (:p (str (the main-section main-div)))
				      (:p (the (:write-back-link :display-string "Home"))))))
  
  :functions ((delete-transaction!
	       (index)
	       (the ledger transactions (delete! index))
	       (the ledger save-transactions!)))

  :objects ((delete-buttons  :type 'button-form-control
			     :sequence (:size (length (the transactions)))
			     :label "Delete"
			     :onclick 
			     (the (gdl-ajax-call 
				   :function-key :delete-transaction!
				   :arguments (list (the-object (nth (the-child index)
								     (the transactions))
								index)))))

	    (main-section 
	     :type 'sheet-section
	     :inner-html (with-cl-who-string ()
			   ((:table :bgcolor :black)
			    ((:tr :bgcolor :yellow) 
			     (dolist (heading (rest (the headings)))
			       (htm (:th (str heading)))) (:th ""))
			    (let ((count -1))
			      (dolist (transaction (the transactions))
				(incf count)
				(htm 
				 ((:tr :bgcolor (gethash :grey-light-very 
							 *color-table*))
				  (dolist (slot (list :from-account :to-account 
						      :date :amount :payee ))
				    (let* ((raw-value 
					    (the-object transaction (evaluate slot)))
					   (value (case slot 
						    ((:from-account :to-account)
						     (the :accounts-sequence 
							  (get-member raw-value) :name))
						    (:date (iso-date raw-value))
						    (:amount (number-format raw-value 2))
						    (otherwise raw-value))))
				      (htm ((:td :align (case slot (:amount :right)
							      (otherwise :left))) 
					    (case slot (:amount 
							(htm 
							 (:tt (fmt "$~$" value))))
						  (otherwise 
						   (htm (str value))))))))
				 
				  (:td (str (the (delete-buttons count) 
						 form-control-string))))))))))))
	     



(define-object add-transaction (base-ajax-sheet)
  :input-slots (accounts-sequence main-sheet)
  
  :computed-slots ((respondent (the main-sheet)))
		   
  :functions
  ((write-html-sheet
    ()
    (html 
     (:html 
      (:head (:title "Add Transaction"))
      (:body (:h2 (:center "Add Transaction"))
	     (:p (the (:write-back-link :display-string "Home")))
	     (with-html-form ()
	       (:p ((:table :bgcolor :black)
		    (:tr ((:td :bgcolor :yellow) "From Account")
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:select :name :from-account)
			   (mapcar #'(lambda(account)
				       (html ((:option :value (the-object account index))
					      (:princ (the-object account name)))))
				   (list-elements (the accounts-sequence))))))
		    (:tr ((:td :bgcolor :yellow) "To Account")
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:select :name :to-account)
			   (mapcar #'(lambda(account)
				       (html ((:option :value (the-object account index))
					      (:princ (the-object account name)))))
				   (list-elements (the accounts-sequence))))))
		    (:tr ((:td :bgcolor :yellow) "Date")
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :type :text :name :date 
				   :size 12 :value (iso-date (get-universal-time))))))
		    (:tr ((:td :bgcolor :yellow) "Amount") 
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :name :amount :type :text :size 15))))
		    (:tr ((:td :bgcolor :yellow) "Payee") 
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :name :payee :type :text :size 30))))))
	       (:p ((:input :type :submit :name :add-transaction :value " Add! "))))))))))


(define-object add-account (base-ajax-sheet)
  :input-slots (main-sheet)
  
  :computed-slots ((respondent (the main-sheet)))
  
  :functions
  ((write-html-sheet
    ()
    (html 
     (:html 
      (:head (:title "Add Account"))
      (:body (:h2 (:center "Add Account"))
	     (:p (the (:write-back-link :display-string "Home")))
	     (with-html-form ()
	       (:p ((:table :bgcolor :black)
		    (:tr ((:td :bgcolor :yellow) "Name")
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :type :text :name :name :size 25))))
		    (:tr ((:td :bgcolor :yellow) "Description") 
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :type :text :name :description :size 40))))
		    (:tr ((:td :bgcolor :yellow) "Number") 
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :type :text :name :account-number :size 25))))
		    (:tr ((:td :bgcolor :yellow) "Type") 
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :type :text :name :account-type :size 25))))
		    (:tr ((:td :bgcolor :yellow) "Class")
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:select :name :account-class :size 1)
			   ((:option :value "Asset/Liability") "Asset/Liability")
			   ((:option :value "Income/Expense") "Income/Expense"))))
		    (:tr ((:td :bgcolor :yellow) "Beginning Balance")
			 ((:td :bgcolor (gethash :green-spring *color-table*))
			  ((:input :type :text :name :beginning-balance 
				   :size 15 :value 0))))))
	       (:p ((:input :type :submit :name :add-account :value " Add! "))))))))))

		   

(defun iso-to-universal (iso-date)
  (let ((year (read-safe-string (subseq iso-date 0 4)))
	(month (read-safe-string (subseq iso-date 5 7)))
	(date (read-safe-string (subseq iso-date 8 10))))
    (encode-universal-time 1 0 0 date month year)))
	

(defun iso-date (universal-time)
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time universal-time)
    (declare (ignore seconds minutes hours))
    (format nil "~a-~2,,,'0@a-~2,,,'0@a" year month date)))

(publish :path "/ledger"
	 :function #'(lambda(req ent)
		       (gwl-make-part req ent "ledger:html-assembly")))
