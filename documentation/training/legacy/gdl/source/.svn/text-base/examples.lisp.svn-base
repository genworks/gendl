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

