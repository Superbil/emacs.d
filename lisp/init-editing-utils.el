;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t -*-
(use-package unfill)

(use-package elec-pair
  :if (fboundp 'electric-pair-mode)
  :ensure nil
  :config
  (electric-pair-mode))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(use-package autorevert
  :ensure nil
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        auto-revert-mode-text " ♲")
  :config
  (global-auto-revert-mode))

 ;;; A simple visible bell which works in all terminal types
(use-package faces
  :ensure nil
  :preface
  (defun sanityinc/flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.05 nil 'invert-face 'mode-line))
  :config
  (setq-default ring-bell-function 'sanityinc/flash-mode-line))




(use-package simple
  :bind (("RET" . newline-and-indent)
         ("S-<return>" . sanityinc/newline-at-end-of-line)
         ("C-M-<backspace>" . kill-back-to-indentation))
  :ensure nil
  :preface
  (defun sanityinc/newline-at-end-of-line ()
    "Move to end of line, enter a newline, and reindent."
    (interactive)
    (move-end-of-line 1)
    (newline-and-indent))

  (defun kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the line."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (kill-region (point) prev-pos)))

  :config
  (transient-mark-mode t))



(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (use-package subword
    :diminish subword-mode))



(use-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :diminish indent-guide-mode)



(use-package nlinum)



(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)  )



(use-package prog-mode
  :if (fboundp 'global-prettify-symbols-mode)
  :ensure nil
  :config
  (global-prettify-symbols-mode))



(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :diminish undo-tree-mode)


(use-package highlight-symbol
  :after (prog-mode html-mode css-mode)
  :config
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode)))

(use-package highlight-symbol
  :after org-mode
  :hook (org-mode . highlight-symbol-nav-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :config
  (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)



;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(use-package cua-base
  :ensure nil
  ;; for rectangles, CUA is nice
  :config
  (cua-selection-mode t))

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;;----------------------------------------------------------------------------
;; Quick jump to word or char
;;----------------------------------------------------------------------------
(use-package avy
  :bind (("C-;" . avy-goto-word-or-subword-1)
         ("C-:" . avy-goto-char))
  :config
  (avy-setup-default))

;;----------------------------------------------------------------------------
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c m a" . mc/edit-beginnings-of-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m c" . mc/edit-lines)
         ("C-c m m" . mc/mark-all-like-this-dwim)
         ("C-c m s" . mc/mark-sgml-tag-pair)
         ("C-c m d" . mc/mark-all-like-this-in-defun)
         ("C-c m r" . set-rectangular-region-anchor)))


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines
  :config
  (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(use-package move-dup
  :bind
  (([M-up] . md/move-lines-up)
   ([M-down] . md/move-lines-down)
   ([M-S-up] . md/move-lines-up)
   ([M-S-down] . md/move-lines-down)

   ("C-c d" . md/duplicate-down)
   ("C-c D" . md/duplicate-up)))

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region
  :preface
  (defun suspend-mode-during-cua-rect-selection (mode-name)
    "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
    (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
          (advice-name (intern (format "suspend-%s" mode-name))))
      (with-eval-after-load 'cua-rect
        `(progn
           (defvar ,flagvar nil)
           (make-variable-buffer-local ',flagvar)
           (defadvice cua--activate-rectangle (after ,advice-name activate)
             (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
             (when ,flagvar
               (,mode-name 0)))
           (defadvice cua--deactivate-rectangle (after ,advice-name activate)
             (when ,flagvar
               (,mode-name 1)))))))

  :diminish whole-line-or-region-local-mode
  :config
  (if (fboundp 'whole-line-or-region-mode)
      (whole-line-or-region-mode t)
    (whole-line-or-region-global-mode t))
  (make-variable-buffer-local 'whole-line-or-region-mode)
  (suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode))




(use-package indent
  :bind ("C-o" . sanityinc/open-line-with-reindent)
  :ensure nil
  :preface
  (defun sanityinc/open-line-with-reindent (n)
    "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
    (interactive "*p")
    (let* ((do-fill-prefix (and fill-prefix (bolp)))
           (do-left-margin (and (bolp) (> (current-left-margin) 0)))
           (loc (point-marker))
           ;; Don't expand an abbrev before point.
           (abbrev-mode nil))
      (delete-horizontal-space t)
      (newline n)
      (indent-according-to-mode)
      (when (eolp)
        (delete-horizontal-space t))
      (goto-char loc)
      (while (> n 0)
        (cond ((bolp)
               (if do-left-margin (indent-to (current-left-margin)))
               (if do-fill-prefix (insert-and-inherit fill-prefix))))
        (forward-line 1)
        (setq n (1- n)))
      (goto-char loc)
      (end-of-line)
      (indent-according-to-mode))))


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

;;----------------------------------------------------------------------------
;; turn on/off truncate-lines
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x T") 'toggle-truncate-lines)

;;----------------------------------------------------------------------------
;; Use shell-like backspace C-h, rebind help
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-x /") 'help-command)




(use-package highlight-escape-sequences
  :config
  (hes-mode))


(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "M-s" "C-h"))
  :diminish guide-key-mode
  :hook (after-init . guide-key-mode))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
