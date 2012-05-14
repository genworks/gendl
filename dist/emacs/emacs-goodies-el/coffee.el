;;; coffee.el --- Submit a BREW request to an RFC2324-compliant coffee device
;;;
;;; Author: Eric Marsden <emarsden@laas.fr>
;;; Version: 0.3
;;; Copyright: (C) 1999, 2003 Eric Marsden
;;; Keywords: coffee, brew, kitchen-sink, can't
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <emarsden@laas.fr>. 
;; The latest version of this package should be available at
;;
;;     <URL:http://purl.org/net/emarsden/home/downloads/>

;;; Commentary:
;;
;; This module provides an Emacs interface to RFC2324-compliant coffee
;; devices (Hyper Text Coffee Pot Control Protocol, or HTCPCP). It
;; prompts the user for the different additives, then issues a BREW
;; request to the coffee device.
;;
;; coffee.el requires a special BREW-capable version of Emacs/W3 to be
;; installed.
;;
;; Reference: <URL:ftp://ftp.isi.edu/in-notes/rfc2324.txt>
;;
;;
;; Thanks to Giacomo Boffi <giacomo.boffi@polimi.it> for some typos
;; and the addition of the "Brown-Coffee" sweetener type.

;;; Code:

(require 'cl)

(defvar coffee-host "coffee"
  "*The host which provides the coffee service.") 

(defvar coffee-pot-designator 1
  "*On machines with multiple pots, the number of the pot to brew in")

(defvar coffee-brew-hook nil
  "*Hook executed before issuing a BREW request")

(defconst coffee-milk-types
  '("Cream" "Half-and-Half" "Whole-Milk" "Part-Skim" "Skim" "Non-Dairy"))

(defconst coffee-syrup-types '("Vanilla" "Almond" "Raspberry" "Chocolate"))

(defconst coffee-sweetener-types '("White-Sugar" "Brown-Sugar" "Artificial-Sweetener"))

(defconst coffee-alcohol-types '("Whiskey" "Rum" "Kahula" "Aquavit"))

(defconst coffee-addition-types
  `(("Milk"      . ,coffee-milk-types)
    ("Syrup"     . ,coffee-syrup-types)
    ("Sweetener" . ,coffee-sweetener-types)
    ("Alcohol"   . ,coffee-alcohol-types)))

(defun coffee ()
  "Submit a BREW request to an RFC2324-compliant coffee device"
  (interactive)
  (require 'url)
  (let* ((additions-list
          (append coffee-milk-types
                  coffee-syrup-types
                  coffee-sweetener-types
                  coffee-alcohol-types))
         (additions-string
          (mapconcat #'identity additions-list ","))
         (url (coffee-url))
         (url-request-method "BREW")
         (url-request-extra-headers
          `(("Content-type"     . "message-coffeepot")
            ("Accept-Additions" . ,additions-string)))
         (url-request-data "START"))
    (run-hooks 'coffee-brew-hook)
    (url-retrieve url (lambda () (coffee-drink)))))

(defun coffee-additions ()
  (let* ((type-name
          (completing-read "Coffee addition: " coffee-addition-types nil t))
         (type (cdr (assoc type-name coffee-addition-types)))
         (ingredients (mapcar #'(lambda (a) (cons a a)) type))
         (ingredient
          (completing-read "Addition type: " ingredients nil t)))
    ingredient))

(defun coffee-url ()
  (require 'w3-forms)
  (concat "coffee://" coffee-host "/"
          (int-to-string coffee-pot-designator)
          "?" (w3-form-encode-xwfu (coffee-additions))))


(defun coffee-drink ()
  (sleep-for -1))


(provide 'coffee)

;; coffee.el ends here
