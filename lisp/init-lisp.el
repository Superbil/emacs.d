;;; init-lisp.el --- Emacs lisp settings, and common config for other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elisp-slime-nav
  :after elisp-mode
  :diminish
  :hook ((emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
         (ielm-mode . turn-on-elisp-slime-nav-mode)))



(use-package lively)


;; Make C-x C-e run 'eval-region if the region is active

(use-package pp
  :ensure nil
  :preface
  (defun sanityinc/eval-last-sexp-or-region (prefix)
    "Eval region from BEG to END if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (pp-eval-last-sexp prefix)))
  :bind (([remap eval-expression] . pp-eval-expression))

  :config
  (defadvice pp-display-expression (after sanityinc/make-read-only (expression out-buffer-name) activate)
    "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1)))))

(use-package lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-x C-e" . sanityinc/eval-last-sexp-or-region))
  :ensure nil)

(use-package ipretty
  :after pp
  :config
  (ipretty-mode 1))


;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.
(use-package elisp-mode
  :ensure nil
  :preface
  (defun sanityinc/switch-to-ielm ()
    (interactive)
    (let ((orig-buffer (current-buffer)))
      (if (get-buffer "*ielm*")
          (funcall sanityinc/repl-switch-function "*ielm*")
        (ielm))
      (setq sanityinc/repl-original-buffer orig-buffer)))

  (defun sanityinc/maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1)))

  :bind (:map emacs-lisp-mode-map
              ("C-c C-z" . sanityinc/switch-to-ielm))
  :hook (emacs-lisp-mode . sanityinc/maybe-set-bundled-elisp-readonly)
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp"))))

(use-package ielm
  :ensure nil
  :after elisp-mode
  :init
  (defvar sanityinc/repl-original-buffer nil
    "Buffer from which we jumped to this REPL.")
  (make-variable-buffer-local 'sanityinc/repl-original-buffer)
  (defvar sanityinc/repl-switch-function 'switch-to-buffer-other-window)

  :preface
  (defun sanityinc/repl-switch-back ()
    "Switch back to the buffer from which we reached this REPL."
    (interactive)
    (if sanityinc/repl-original-buffer
        (funcall sanityinc/repl-switch-function sanityinc/repl-original-buffer)
      (error "No original buffer")))

  :bind (:map ielm-map
              ("C-c C-z" . sanityinc/repl-switch-back)))

;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))

;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(use-package auto-compile
  :hook
  ((after-init . auto-compile-on-save-mode)
   (after-init . auto-compile-on-load-mode)))

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)


(use-package immortal-scratch
  :hook
  (after-init . immortal-scratch-mode))


;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))


;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defun sanityinc/disable-indent-guide ()
  (when (bound-and-true-p indent-guide-mode)
    (indent-guide-mode -1)))

;; ----------------------------------------------------------------------------
;; Minor mode to aggressively keep your code always indented
;; ----------------------------------------------------------------------------
(use-package aggressive-indent
  :init
  (defvar sanityinc/lispy-modes-hook
    '(enable-paredit-mode
      turn-on-eldoc-mode
      sanityinc/disable-indent-guide
      sanityinc/enable-check-parens-on-save)
    "Hook run in all Lisp modes.")
  :config
  (add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode))

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'sanityinc/lispy-modes-hook))

(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(if (boundp 'eval-expression-minibuffer-setup-hook)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (use-package eldoc-eval
    :config
    (add-hook 'after-init-hook 'eldoc-in-minibuffer-mode)))

(use-package elisp-mode
  :ensure nil
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("\\.emacs-project\\'" . emacs-lisp-mode)
         ("archive-contents\\'" . emacs-lisp-mode)))

(use-package cl-lib-highlight
  :after lisp-mode
  :config
  (cl-lib-highlight-initialize))

;; ----------------------------------------------------------------------------
;; Delete .elc files when reverting the .el from VC or magit
;; ----------------------------------------------------------------------------

;; When .el files are open, we can intercept when they are modified
;; by VC or magit in order to remove .elc files that are likely to
;; be out of sync.

;; This is handy while actively working on elisp files, though
;; obviously it doesn't ensure that unopened files will also have
;; their .elc counterparts removed - VC hooks would be necessary for
;; that.

(defvar sanityinc/vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defadvice revert-buffer (after sanityinc/maybe-remove-elc activate)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when sanityinc/vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))

(defadvice magit-revert-buffers (around sanityinc/reverting activate)
  (let ((sanityinc/vc-reverting t))
    ad-do-it))
(defadvice vc-revert-buffer-internal (around sanityinc/reverting activate)
  (let ((sanityinc/vc-reverting t))
    ad-do-it))



(use-package macrostep
  :after lisp-mode
  :bind (:map emacs-lisp-mode-map ("C-c e" . macrostep-expand)))



;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-x / K") 'find-function-on-key)



;; Extras for theme editing

(defvar sanityinc/theme-mode-hook nil
  "Hook triggered when editing a theme file.")

(defun sanityinc/run-theme-mode-hooks-if-theme ()
  "Run `sanityinc/theme-mode-hook' if this appears to a theme."
  (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
    (run-hooks 'sanityinc/theme-mode-hook)))

(add-hook 'emacs-lisp-mode-hook 'sanityinc/run-theme-mode-hooks-if-theme t)

(use-package rainbow-mode
  :hook
  (sanityinc/theme-mode . rainbow-mode))

(use-package aggressive-indent
  :config
  ;; Can be prohibitively slow with very long forms
  (add-to-list 'sanityinc/theme-mode-hook (lambda () (aggressive-indent-mode -1)) t))



(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))



(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup))



;; ERT
(use-package ert
  :bind (:map ert-results-mode-map
              ("g" . ert-results-rerun-all-tests))
  :ensure nil)


(defun sanityinc/cl-libify-next ()
  "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat
      "("
      (regexp-opt
       ;; Not an exhaustive list
       '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
         "case" "destructuring-bind" "second" "third" "defun*"
         "defmacro*" "return-from" "labels" "cadar" "fourth"
         "cadadr") t)
      "\\_>")))
  (let ((form (match-string 1)))
    (backward-sexp)
    (cond
     ((string-match "^\\(defun\\|defmacro\\)\\*$")
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))


(use-package cask-mode)


(provide 'init-lisp)
;;; init-lisp.el ends here
