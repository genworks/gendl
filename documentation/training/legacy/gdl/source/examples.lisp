;;
;; Copyright 2002, 2009, 2012 Genworks International
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

(in-package :gdl-tutorial)

(define-object assembly-1 (base-object)
  
   :input-slots
   ((account-data-file "~/genworks/gwl-apps/ledger/data/accounts.lisp")
    (transaction-data-file "~/genworks/gwl-apps/ledger/data/transactions.lisp"))
  
   :computed-slots 
   ((account-data (let (result)
                    (with-open-file (in (the account-data-file))
                      (do ((account (read in nil nil) (read in nil nil)))
                          ((null account) (nreverse result)) 
                        (push account result)))))
   
    (transaction-data (let (result)
                        (with-open-file (in (the transaction-data-file))
                          (do ((transaction (read in nil nil) (read in nil nil)))
                              ((null transaction) (nreverse result)) 
                            (push transaction result)))))
   
    (account-indices (mapcar #'(lambda(account) (the-object account index)) 
                             (list-elements (the accounts))))
   
    (transaction-indices (mapcar #'(lambda(transaction) 
                                     (the-object transaction index)) 
                                 (list-elements (the transactions))))
   
    (net-worth (let ((result 0))
                 (dolist (account (list-elements (the accounts)) result)
                   (when (eql (make-keyword (the-object account account-class)) 
                              :asset/liability)
                     (incf result (the-object account current-balance))))))
   
    (profit (let ((result 0))
              (dolist (account (list-elements (the accounts)) result)
                (when (eql (make-keyword (the-object account account-class)) 
                           :income/expense)
                  (decf result (the-object account current-balance))))))
   
    (balances-hash-table (let ((ht (make-hash-table)))
                           (dolist (account (list-elements (the accounts)))
                             (setf (gethash (the-object account index) ht)
                               (the-object account beginning-balance)))
                           (dolist (transaction (list-elements 
                                                 (the transactions)) ht)
                             (decf 
                              (gethash (the-object transaction from-account) ht)
                              (the-object transaction amount))
                             (incf 
                              (gethash (the-object transaction to-account) ht)
                              (the-object transaction amount)))))))

