;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode)
         (after-save . executable-make-buffer-file-executable-if-script-p)))

(setq goto-address-mail-face 'link)

(setq-default regex-tool-backend 'perl)

(add-auto-mode 'conf-mode "Procfile")

;;; Placing all backup files in temp directory
;; http://www.emacswiki.org/emacs-en/BackupDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;----------------------------------------------------------------------------
;; ASCII table
;;----------------------------------------------------------------------------
(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (setq lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
                  "bs" "ht" "nl" "vt" "np" "cr" "so" "si"
                  "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
                  "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
                  ))
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (elt lower32 i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96))))))

;;----------------------------------------------------------------------------
;; Uniq lines
;;----------------------------------------------------------------------------
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

;;----------------------------------------------------------------------------
;; Ping DNS
;;----------------------------------------------------------------------------
(defun ping-google-dns ()
  "Ping google's DNS."
  (interactive)
  (ping "8.8.8.8"))


(provide 'init-misc)
;;; init-misc.el ends here
