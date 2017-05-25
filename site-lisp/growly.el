;;; growl.el --- Growl notifications

;; Copyright (C) 2006 BT Templeton
;;;              2015 Superbil

;; Author: BT Templeton <bpt@tunes.org>
;;         Superbil <superbil+develop@Gmail.com>

;; Keywords: growl notification mac osx
;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; 2015-01-06 Fork from http://www.emacswiki.org/emacs/growl.el

;;; Code:

(defvar growl-program "growlnotify")

(defun growl (title message)
  "Send TITLE and MESSAGE to growl-program."
  (start-process "growl" " growl"
                 growl-program
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

(provide 'growl)
;;; growly.el ends here
