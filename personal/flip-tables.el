;;; flip-tables.el --- put a flipping table emoji on your kill ring
(defconst flip-tables-version "0.1")
;; Copyright (c)2008 Jonathan Arkell. (by)(nc)(sa)  Some rights reserved.
;; Author: Jonathan Arkell <jonnay@jonnay.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
(defgroup flip-tables '()
  "")
  
;;; Installation:
;; Put flip-tables.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;(require 'flip-tables)


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `flip-table'
;;    (╯°□°）╯︵ ┻━┻ 
;;  `flip-angry-table'
;;    (ノ□益□)ノ彡┻━┻ 
;;  `flip-it-back'
;;    ┬──┬ ﻿ノ( ゜-゜ノ)
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;


;;; elisp:
;; 

;;; TODO:
;; - alow prefix argument to insert a flipping table, instead of just push it on the kill ring
;; - pass in a type to the table i.e.  PHP: ┻━dHd━┻    jQuery: ┻━$━┻
;;      NOTE.  no unflipping for these types will be provided.  You'd best just burn the table. 

;;; CHANGELOG:
;; v 0.1 - Initial release

;;; Code:



(defun flip-table ()
  "(╯°□°）╯︵ ┻━┻ "
  (interactive)
  (message "( °_°） ┬──┬ ")
  (sleep-for 0 500)
  (message "(╯°□°）╯︵ ┻━━┻ ")
  (kill-new "(╯°□°）╯︵ ┻━━┻ "))

(defun flip-angry-table ()
  "(ノ□益□)ノ彡┻━┻ "
  (interactive)
  (message "( °_°） ┬──┬")
  (sleep-for 0 500)
  (message "(-°□°）-┬──┬ ")
  (sleep-for 0 250)
  (message "(ノ□益□)ノ彡┻━━┻ ")
  (kill-new "(ノ□益□)ノ彡┻━━┻ "))

(defun flip-it-back ()
  "┬──┬ ﻿ノ( ゜-゜ノ)"
  (interactive)
  (message "┻━━┻    ( -.-  )")
  (sleep-for 0 500)
  (message "┻━━┻  ノ( ゜-゜ノ)")
  (sleep-for 0 250)
  (message "┬──┬  ノ( ゜-゜ノ)")
  (sleep-for 0 250)
  (message "┬──┬    (  ^_^  )")
  (kill-new "┬──┬ ﻿ノ( ゜-゜ノ)"))


(provide 'flip-tables)

;;; flip-tables ends here