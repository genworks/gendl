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

(defparameter *source-dir*  (glisp:system-home :ledger))

(define-object assembly (base-object)
  
  :input-slots
  ((account-data-file (merge-pathnames "data/accounts.lisp" *source-dir*))
   (transaction-data-file (merge-pathnames "data/transactions.lisp" *source-dir*)))
  
  :computed-slots 
  ((account-data (let (result)
		   (with-open-file (in (the account-data-file))
		     (do ((account (read in nil nil) (read in nil nil)))
			 ((null account) (nreverse result)) 
		       (push account result)))))
   
   (transaction-data (let (result)
		       (with-open-file 
			   (in (the transaction-data-file))
			 (do ((transaction (read in nil nil) (read in nil nil)))
			     ((null transaction) 
			      (nreverse result)) 
			   (push transaction result)))))
   
   (account-indices (mapcar 
		     #'(lambda(account) 
			 (the-object account index)) 
		     (list-elements (the accounts))))
   
   (transaction-indices (mapcar 
			 #'(lambda(transaction) 
			     (the-object transaction index)) 
			 (list-elements (the transactions))))
   
   (net-worth (let ((result 0))
		(dolist (account (list-elements (the accounts)) result)
		  (print-variables result)
		  (print-variables (make-keyword (the-object account account-class)))
		  (when (eql (make-keyword (string-downcase (the-object account account-class)) )
			     :asset/liability)
		    (incf result (the-object account current-balance))))))
   
   (profit (let ((result 0))
	     (dolist (account (list-elements (the accounts)) result)
	       (when (eql (make-keyword (string-downcase (the-object account account-class)) )
			  :income/expense)
		 (decf result (the-object account current-balance))))))
   
   (balances-hash-table (let ((ht (make-hash-table)))
			  (dolist (account (list-elements (the accounts)))
			    (print-variables account)
			    (setf (gethash (the-object account index) ht)
			      (the-object account beginning-balance)))
			  (dolist (transaction (list-elements 
						(the transactions)) ht)
			    
			    (print-variables transaction)
			    
			    (let ((amount (the-object transaction amount)))
			      (print-variables amount))
			    
			    (decf 
			     (gethash (the-object transaction from-account) ht)
			     (the-object transaction amount))
			    (incf 
			     (gethash (the-object transaction to-account) ht)
			     (the-object transaction amount))))))
  
  :objects
  ((accounts :type 'account
	     :sequence (:indices (mapcar #'first (rest (the account-data))))
	     :data (nth (the-child index) (rest (the account-data)))
	     :current-balance (gethash (the-child index) 
				       (the balances-hash-table))
	     :headings (first (the account-data)))
   
   (transactions :type 'transaction
		 :sequence (:indices (mapcar #'first 
					     (rest (the transaction-data))))
		 :data (nth (the-child index) (rest (the transaction-data)))
		 :headings (first (the transaction-data))))
  
  :functions
  (
   (add-transaction! 
    (&key from-account to-account date amount payee)
    (print-variables from-account to-account date amount payee)
    (when (or (not (member from-account (the account-indices)))
	      (not (member to-account (the account-indices)))
	      (not (numberp amount)) (not (stringp payee))
	      (not (ignore-errors (decode-universal-time date))))
      (error "One or more invalid arguments given to add-transaction!"))
    (let ((new-index (1+ (if (null (the transaction-indices)) 0 
			   (apply #'max (the transaction-indices))))))
      (the transactions (insert! new-index))
      (the (transactions new-index))
      (the (transactions new-index) 
	(set-slot! :data (list new-index from-account 
			       to-account date amount payee)))
      
      (let ((new-transaction (the (transactions new-index))))
	(print-variables new-transaction new-index)))
    
    
    (the save-transactions!))

   
   (save-transactions!
    ()
    (with-open-file (out (the transaction-data-file) 
		     :direction :output :if-exists :supersede 
		     :if-does-not-exist :create)
      (print (first (the transaction-data)) out)
      (dolist (transaction (list-elements (the transactions)))
	(print (the-object transaction data) out))))
   
   (add-account! 
    (&key name description account-number account-type account-class beginning-balance)
    (when (or (not (member account-class (list "Asset/Liability" "Income/Expense")
			   :test #'string-equal))
	      (not (numberp beginning-balance)))
      (error "One or more invalid arguments given to add-account!"))
    (let ((new-index (1+ (if (null (the account-indices)) 0 
			   (apply #'max (the account-indices))))))
      (the accounts (insert! new-index))
      (the (accounts new-index))
      (the (accounts new-index) 
	(set-slot! :data (list new-index name description account-number account-type
			       account-class beginning-balance))))
    (the save-accounts!))

   
   (save-accounts!
    ()
    (with-open-file (out (the account-data-file)
		     :direction :output :if-exists :supersede 
		     :if-does-not-exist :create)
      (print (first (the account-data)) out)
      (dolist (account (list-elements (the accounts)))
	(print (the-object account data) out))))))

  
(define-object account (base-object)
  :input-slots
  (data headings current-balance)
  
  :computed-slots
  ((name (second (the data)))
   (description (third (the data)))
   (account-number (fourth (the data)))
   (account-type (fifth (the data)))
   (account-class (sixth (the data)))
   (beginning-balance (seventh (the data)))))

(define-object transaction (base-object)
  :input-slots
  (data headings)
  
  :computed-slots
  ((from-account (second (the data)))
   (to-account (third (the data)))
   (date (fourth (the data)))
   (amount (fifth (the data)))
   (payee (sixth (the data)))))

  
