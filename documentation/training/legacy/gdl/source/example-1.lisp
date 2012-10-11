(in-package :gdl-tutorial)

(defparameter *example-1*
    `((:chapter :title "Example 1: Personal Ledger")
      
      "In this chapter we will describe a simple personal accounting ledger application. 
First we will build the core objects necessary to keep track of accounts and transactions; 
then we will layer a web user interface on top of these objects to allow for convenient
end-user access. 

I have chosen to build this application in base GDL/GWL, without the use of any database"
      (:footnote (:indexed "Paul Graham")
		 " is fond of reminding us that ``the filesystem is already a database.''")
      " or other general-purpose mixins. Clearly, one could greatly reduce the amount of code
required for an application like this by using ``utility'' mixins for tasks such as database or
filesystem access and standard GUI templates. 

While it certainly does not implement the most efficient accounting/ledger algorithms possible, 
this application will give us a small taste of the power of the caching and dependency-tracking
features of a KB system."
      
      ((:section :title "Main Ledger Object")
       
       "The full source for the Ledger application is available in the GDL application directory under the "
       (:texttt "gwl-samples")
       " directory. We include partial hardcopy in this tutorial, but if you wish to try running the example 
yourself you should use the code from the CD rather than trying to type it in from this hardcopy (also, 
some changes may have occured since the preparation of this tutorial). 

The ledger application needs write access to files under the "
       (:texttt "gwl-samples/ledger/data/")
       " directory, so you may need to open up permissions on these files, or copy the entire "
       (:texttt "ledger/")
       " directory to a location where you have write access, such as your home directory. 

Figure "
       (:ref "code:ledger-input-computed")
       " shows the input-slots and computed-slots of our main object, named conventionally 
``assembly'' (in the "
       (:texttt ":ledger")
       " Lisp package). The two inputs each default to a file holding the beginning 
data set for Accounts and Transactions respectively. Figure "
       (:ref "data:ledger-data")
       " shows a sampling of the first few lines from typical data files. 
These data files are given the ``lisp'' extension simply so that they will 
come up in Lisp-mode in Emacs for ease of hand-editing -- they are not
meant to be compiled as Lisp program source code.

The "
       (:texttt "account-data")
       " and "
       (:texttt "transaction-data")
       " computed-slots read and hold the contents of these data files for use in 
initializing the actual "
       (:texttt "accounts")
       " and "
       (:texttt "transactions")
       " object sequences, which the ledger application will use. The "
       (:texttt "account-indices")
       " and "
       (:texttt "transaction-indices")
       " collect up the unique indices from the individual acounts and transactions, 
used to compute the ``next'' index when adding a new ``record.'' The "
       (:texttt "net-worth")
       " and "
       (:texttt "profit")
       " slots compute the sums of Asset/Liability accounts and Income/Expense accounts, 
respectively (the sign on "
       (:texttt "profit")
       " is reversed to make Income items appear positive and 
Expense items appear negative). Finally, the "
       (:texttt "balances-hash-table")
       " slot is a hash table keyed on the account indices, computing the current balance
of each account. This value is passed into the actual account objects for later use."
       
       
       ((:boxed-figure :caption "Contents of Account and Transaction Data Files"
		       :label "data:ledger-data")
	(:verbatim "


 (\"Index\" \"Name\" \"Description\" \"Account Number\" 
  \"Account Type\" \"Account Class\" \"Beginning Balance\") 
 (0 \"DFCU\" \"Dearborn Federal Credit Union\" \"999-6969-999\" 
  \"Checking\" \"Asset/Liability\" 25000) 
 (1 \"Waterhouse\" \"Waterhouse Taxable\" \"555-7979-555\" 
  \"Savings\" \"Asset/Liability\" 12500) 

   ...


 (\"Index\" \"From Account\" \"To Account\" \"Date\" \"Amount\" \"Payee\") 
 (0 0 1 3243976597 1000 \"djc\") 
 (1 0 1 3243976772 1500 \"djc\")

   ...
"))
	
       
       
       (:define-object assembly-1)
       
       ((:boxed-figure :caption "Input-Slots and Computed-Slots of Ledger"
                       :label "code:ledger-input-computed")
	
	(:small (:define-object assembly-1))
		    
		    
	#+nil
        (:small (:verbatim  "


 (define-object assembly (base-object)
  
   :input-slots
   ((account-data-file \"~/genworks/gwl-apps/ledger/data/accounts.lisp\")
    (transaction-data-file \"~/genworks/gwl-apps/ledger/data/transactions.lisp\"))
  
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
                              (the-object transaction amount)))))) ...
")))

       
       
       "Figure "
       (:ref "fig:genacc2-subobjects")
       " shows the two sub-objects contained within the ledger assembly object. Each of these
is specified as a "
       (:texttt (:indexed "sequence"))
       ", based on the initial data read from the data files. Because these sequences are specified
by a list of "
       (:texttt (:indexed ":indices"))
       ", rather than by a fixed "
       (:texttt (:indexed ":size"))
       ", they can be programmatically modified by inserting and deleting sequence elements. In 
this example, such insertions or deletions would be analogous to row operations on a relational
database table. 

Note that the "
       (:texttt "current-balance")
       " is accessed from the "
       (:texttt "balances-hash-table")
       ", a computed-slot listed in Figure "
       (:ref "code:ledger-input-computed")
       "."
       
       ((:boxed-figure :caption "Sub-Objects of Ledger"
                       :label "fig:genacc2-subobjects")
        (:small (:verbatim "


    ...
       
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
  

    ...
")))
       
       "The final segment of our main ledger object is listed in Figure "
       (:ref "fig:genacc2-functions")
       ". Here, we specify some functions on the ledger object which accept
arguments and perform some side-effect on the object. Note that GDL "
       (:texttt ":functions")
       " are distinguished from "
       (:texttt ":computed-slots")
       " by two main characteristics:"
       
       ((:list :style :enumerate)
        (:item "They can accept arguments, a ``lambda list,'' as with a normal CL function.")
        (:item "Their return-values are not cached or dependency-tracked. Their bodies are 
evaluated every time they are referenced (called)."))
       
       "The "
       (:texttt "add-transaction!")
       " function adds a new transaction element to the "
       (:texttt "transactions")
       " object sequence, and immediately calls the "
       (:texttt "save-transactions!")
       " function to update the transactions data file"
       (:footnote "In a more robust application, the in-memory insertion and
the updating of the external file should be done as an indivisible unit 
with ``unwind'' capability, to ensure that either the whole thing succeeds 
or the whole thing fails.")
       ". 

The most important thing to note here is that when a new transaction is added
using the "
       (:texttt "add-transaction!")
       " function, any other message (e.g. computed-slot, object, etc.) which in any
way depends upon the "
       (:texttt "transactions")
       (:index "recomputation!on-demand")
       " sequence of objects will now automatically recompute and return a fresh
value the next time it is demanded. For example, the "
       (:texttt "profit")
       " and "
       (:texttt "net-worth")
       " messages will now return updated values the next time they are demanded.
But the recomputation of a message will happen if and only if the message is
actually demanded. In a very large object tree, for example, thousands of messages
in hundreds of objects might depend on a certain value. 

When that value is 
changed, however, the system "
       (:underline "does not")
       " incur the computational overhead of 
updating all these thousands of dependent items at that time. Perhaps only a 
few dozen of these thousands of dependent items will ever be accessed. Only 
those few dozen will need to be computed.

This is one of the obvious distinctions between a conventional ``spreadsheet'' 
application and a knowledge base application --- a spreadsheet is generally not 
scalable to very large problems or models, because changing a value forces the 
user to wait for an all-or-nothing update of the entire sheet."
       
       (:index "spreadsheet!KB as distinct from")
       
       ((:boxed-figure :caption "Functions of Ledger"
                       :label "fig:genacc2-functions")
        (:small (:verbatim "


   ...

  :functions
  ((add-transaction! (&key from-account to-account date amount payee)
    (when (or (not (member from-account (the account-indices)))
              (not (member to-account (the account-indices)))
              (not (numberp amount)) (not (stringp payee))
              (not (ignore-errors (decode-universal-time date))))
      (error \"One or more invalid arguments given to add-transaction!\"))
    (let ((new-index (1+ (if (null (the transaction-indices)) 0 
                           (apply #'max (the transaction-indices))))))
      (the transactions (insert! new-index))
      (the (transactions new-index) 
        (set-slot! :data (list new-index from-account 
                               to-account date amount payee))))
    (the save-transactions!))
   
   (save-transactions! ()
    (with-open-file (out (the transaction-data-file) 
                     :direction :output :if-exists :supersede 
                     :if-does-not-exist :create)
      (print (first (the transaction-data)) out)
      (dolist (transaction (list-elements (the transactions)))
        (print (the-object transaction data) out))))

   
      ;; ... Similarly for add-account! and save-accounts!

   )))
"))))
      
      
      ((:section :title "Objects for Accounts and Transactions")
       
       "Figure "
       (:ref "fig:genacc2-accountandtransaction")
       " shows the object definitions for "
       (:texttt "account")
       " and "
       (:texttt "transaction")
       ". These are simple objects which essentially receive some inputs and make them
available as messages."
       
       ((:boxed-figure :caption "Account and Transaction Object Definitions"
                       :label "fig:genacc2-accountandtransaction")
        (:verbatim "


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
")))
      
      ((:section :title "Using the Main Ledger Object")
       "We can use the ledger object we have put together so far by typing commands at the 
Common Lisp prompt. First, we will make an instance of a ledger object, conveniently 
setting this instance to the variable "
       (:texttt "self")
       ":"
       (:verbatim "
LEDGER(10): (setq self (make-object 'assembly))
#<ASSEMBLY @ #x7367516a>")
       "Now we can use ``the'' referencing to call various messages in this instance:"
       (:verbatim "
LEDGER(11): (the profit)
140
LEDGER(12): (the net-worth)
37640
LEDGER(13): (the balances-hash-table)
#<EQL hash-table with 7 entries @ #x7367fc1a>
LEDGER(14): (the (accounts 0) current-balance)
28140
LEDGER(15):")
       "Now we will add a transaction, and confirm that it affects our "
       (:texttt "profit")
       " and "
       (:texttt "net-worth")
       ":"
       (:verbatim "
LEDGER(15): (the (add-transaction! :from-account 0 :to-account 4 
                                   :date (get-universal-time) 
                                   :amount 250 :payee \"djc\"))
NIL
LEDGER(16): (the profit)
-110
LEDGER(17): (the net-worth)
37390
LEDGER(18):")                             
       "If we had wrapped the CL "
       (:index "time!using to understand KB dynamics")
       (:texttt "time")
       " macro around for example the call to the "
       (:texttt "profit")
       ", we would have been able to see that indeed it was recomputed the second time
we called it, since something it depends upon had been modified. If we call it again
immediately, without having changed anything, "
       (:texttt "time")
       " would show us that it returns virtually instantaneously without causing any
substantial work to be done, since its value is now cached and the cache is still fresh.")

      ((:section :title "Making a Web Interface with GWL")      
       "Now that we have built and tested our main ledger ``engine,'' let's make it more 
accessible to casual users by layering a web user interface on it. One way to do this is
to create a new toplevel object which "
       (:emph "contains")
       " the ledger engine, and specifies an assembly of objects to represent the web
pages in our interface. Figure "
       (:ref "code:ledger-html-top")
       " shows the definition of the top level, or ``home page,'' of our web application.
 
It contains the actual ledger assembly, or ``engine,'' as well as child objects, which
represent sub-pages in our website, corresponding to an account listing, a transaction
listing, a form for adding a transaction, and a form for adding an account."

       
       ((:boxed-figure :caption "Slots and Object for Web Interface"
                       :label "code:ledger-html-top")
        (:verbatim "


 (define-object html-assembly (base-html-sheet)
  
   :computed-slots 
   ((accounts-ht (let ((ht (make-hash-table)))
                   (dolist (account (list-elements 
                                     (the ledger accounts)) ht)
                     (setf (gethash (the-object account index) ht) 
                       account)))))
  
   :objects
   ((ledger :type 'assembly)
  
    (view-accounts :type 'view-accounts
                   :account-sequence (the ledger accounts))
   
    (view-transactions :type 'view-transactions
                       :transaction-sequence (the ledger transactions)
                       :pass-down (accounts-ht))
   
    (add-transaction :type 'add-transaction
                     :main-sheet self
                     :pass-down (accounts-ht))
   
    (add-account :type 'add-account
                 :main-sheet self))
  ...
"))
       
       "Our toplevel user interface sheet also contains a "
       (:texttt "write-html-sheet")
       " presentation function, shown in Figure "
       (:ref "code:ledger-html-bottom")
       ". This presentation function also serves as a response function
to the forms for adding accounts and transactions -- for this reason, 
it contains two blocks of code, before the actual html page generation,
which take care of actually processing any added transaction or account.

The form values for these will show up as plist values in the "
       (:texttt "query-plist")
       " slot, since they are not specified as named slots in the 
objects which generate the forms (the transaction form is shown 
in Figure "
       (:ref "code:add-transactions-sheet")
       ", and the accounts form is not listed here but is similar)."
       
       ((:boxed-figure :caption "Output Function for Web Interface"
                       :label "code:ledger-html-bottom")
        (:small (:verbatim "


  :functions
  ((write-html-sheet
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
      (:head (:title \"Personal Ledger\"))
      (:body 
       (:h2 (:center \"Personal Ledger\"))
       (:p (:table 
            (:tr (:td \"Net Worth:\")
                 ((:td :align :right) 
                  (:b (:tt ((:font 
                             :color (gethash (if (minusp (the ledger net-worth)) 
                                                 :red :green-lime) *color-table*))
                            (:princ (number-format (the ledger net-worth) 2)))))))
            (:tr (:td \"Profit/Loss:\")
                 ((:td :align :right)
                  (:b (:tt ((:font 
                             :color (gethash (if (minusp (the ledger profit)) 
                                                 :red :green-lime) *color-table*))
                            (:princ (number-format (the ledger profit) 2)))))))))
       (:p (:ul (:li (the view-accounts 
                       (write-self-link :display-string \"View Accounts\")))
                (:li (the view-transactions 
                       (write-self-link :display-string \"View Transactions\")))
                (:li (the add-transaction 
                       (write-self-link :display-string \"Add Transaction\")))
                (:li (the add-account 
                       (write-self-link :display-string \"Add Account\")))))))))))
")))
  
       
       ((:image-figure :image-file "ledger-main.png"
                       :caption "Main Screen of Ledger"
                       :label "fig:ledger-main"))

       ((:image-figure :image-file "transaction-listing.png"
                       :caption "Transaction Listing Sheet"
                       :label "fig:transaction-listing"))
       
       

       
       "Figure "
       (:ref "code:view-transactions-sheet")
       " defines an object which takes two inputs and computes 
two simple slots, but most of all it defines a presentation method to 
generate an html table listing all transactions entered to date. An example 
is seen in Figure "
       (:ref "fig:transaction-listing")
       "."
       ((:boxed-figure :caption "Sheet for Transaction Listing"
		       :label "code:view-transactions-sheet")
	(:verbatim "


 (define-object view-transactions (base-html-sheet)
   :input-slots (transaction-sequence accounts-sequence)

   :computed-slots ((transactions (list-elements 
                                   (the transaction-sequence)))
                    (headings (the-object (first (the transactions)) 
                                          headings)))
   :functions
   ((write-html-sheet
     ()
     (html 
      (:html 
       (:head (:title \"Transaction Listing\"))
       (:body 
        (:h2 (:center \"Transaction Listing\"))
        (:p (the (:write-back-link)))
        (:p ((:table :bgcolor :black)
             ((:tr :bgcolor :yellow) 
              (dolist (heading (rest (the headings)))
                (html (:th (:princ heading)))))
             (dolist (transaction (the transactions))
               (html 
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
                     (html ((:td :align (case slot (:amount :right)
                                              (otherwise :left))) 
                            (case slot (:amount 
                                        (html 
                                         (:tt (format *html-stream* 
                                                      \"$~$\" value))))
                                  (otherwise 
                                   (html (:princ value)))))))))))))
        (:p (the (:write-back-link)))))))))
"))
      

       "Figure "
       (:ref "code:add-transactions-sheet")
       " defines a presentation function to create a fillout-form for adding
a transaction, and Figure "
       (:ref "fig:add-transaction")
       " shows a sample rendition of this form."
      
       ((:boxed-figure :caption "Form for Adding a Transaction"
		       :label "code:add-transactions-sheet")
	(:small (:verbatim "


 (define-object add-transaction (base-html-sheet)
   :input-slots (accounts-sequence main-sheet)

   :computed-slots ((respondent (the main-sheet)))
   
   :functions
   ((write-html-sheet
     ()
     (html 
      (:html 
       (:head (:title \"Add Transaction\"))
       (:body 
        (:h2 (:center \"Add Transaction\"))
        (:p (the (:write-back-link)))
        (with-html-form
         (:p 
          ((:table :bgcolor :black)
           (:tr 
            ((:td :bgcolor :yellow) \"From Account\")
            ((:td :bgcolor (gethash :green-spring *color-table*))
             ((:select :name :from-account)
              (mapcar #'(lambda(account)
                          (html ((:option 
                                  :value (the-object account index))
                                 (:princ (the-object account name)))))
                      (list-elements (the accounts-sequence))))))
           (:tr 
            ((:td :bgcolor :yellow) \"To Account\")
            ((:td :bgcolor (gethash :green-spring *color-table*))
             ((:select :name :to-account)
              (mapcar #'(lambda(account)
                          (html ((:option 
                                  :value (the-object account index))
                                 (:princ (the-object account name)))))
                      (list-elements (the accounts-sequence))))))
           (:tr 
            ((:td :bgcolor :yellow) \"Date\")
            ((:td :bgcolor (gethash :green-spring *color-table*))
             ((:input :type :text :name :date :size 12 
                      :value (iso-date (get-universal-time))))))
           (:tr ((:td :bgcolor :yellow) \"Amount\") 
                ((:td :bgcolor (gethash :green-spring *color-table*))
                 ((:input :name :amount :type :text :size 15))))
           (:tr ((:td :bgcolor :yellow) \"Payee\") 
                ((:td :bgcolor (gethash :green-spring *color-table*))
                 ((:input :name :payee :type :text :size 30))))))
         (:p ((:input :type :submit :name :add-transaction 
                      :value \" Add! \"))))))))))      
")))
       
       ((:image-figure :image-file "add-transaction.png"
                       :caption "Form for Adding a Transaction"
                       :label "fig:add-transaction"))
       
       
       )
      
      ((:section :title "Summary")
       "
Please note that we have built this ledger using only core GDL/GWL,
for illustrative purposes. Several parts of the app have been written
from scratch, which otherwise could be handled by using simple
utilities or libraries for things such as filesystem/database access
and user interface templates.

"
       (:index "mainstream apps!KB technology useful for")       
       (:index "requirements!ever-expanding")

"The main point of this example is to show that KB technology can be
useful even for applications which might be considered ``simple'' or
``mainstream'' --- even the simplest of applications tend to grow
ever-changing and ever-expanding requirements, and it pays in the 
end to use a development environment which can absorb these
requirements gracefully and with ease.")))

      

      
      
       
       


