;;; init-javascript.el --- Support for Javascript and derivatives -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl-lib))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; js2-mode

(use-package js2-mode
  :init
  ;; Change some defaults: customize them to override
  (setq-default js2-basic-offset preferred-javascript-mode
                js2-bounce-indent-p nil)

  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)

  :preface
  ;; ... but enable it if flycheck can't handle javascript
  (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
    (unless (flycheck-get-checker-for-buffer)
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)
      (when (derived-mode-p 'js-mode)
        (js2-minor-mode 1))))

  :config
  (use-package flycheck
    :after flycheck-get-checker-for-buffer
    :config
    (add-hook 'js-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
    (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive))

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (js2-imenu-extras-setup)

  ;; In Emacs >= 25, the following is an alias for js-indent-level anyway
  (setq-default js2-basic-offset preferred-javascript-indent-level))

;; js-mode
(use-package js
  :ensure nil
  :mode ("node" . preferred-javascript-mode)
  :init
  (setq-default js-indent-level preferred-javascript-indent-level))


;; Javascript nests {} and () a lot, so I find this helpful

(use-package xref-js2
  :if (or (executable-find "rg") (executable-find "ag"))
  :preface
  (defun sanityinc/enable-xref-js2 ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

  :config
  (when (executable-find "rg")
    (setq-default xref-js2-search-program 'rg))

  (use-package js
    :bind ("M-." . js-mode-map)
    :hook (js-mode-hook . sanityinc/enable-xref-js2))

  (use-package js2-mode
    :bind ("M-." . js2-mode-map)
    :hook (js2-mode-hook . sanityinc/enable-xref-js2)))



;;; Coffeescript

(use-package coffee-mode
  :after js2-mode
  :mode ("\\.coffee\\.erb\\'" . coffee-mode)
  :config
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))


;;; Livescript

(use-package livescript-mode
  :bind (:map livescript-mode-map
              ("C-c C-l" . livescript-compile-buffer)
              ("C-c C-r" . livescript-compile-region))
  :mode "\\.ls\\'")


;;; TypeScript

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))


;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(use-package js-comint
  :after (js-mode js2-mode)
  :init
  (setq inferior-js-program-command "js")
  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  :bind
  (:map inferior-js-minor-mode-map
        ("\C-x\C-e" . js-send-last-sexp)
        ("\C-\M-x" . js-send-last-sexp-and-go)
        ("\C-cb" . js-send-buffer)
        ("\C-c\C-b" . js-send-buffer-and-go)
        ("\C-cl" . js-load-file-and-go))

  :config
  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(use-package skewer-mode
  :config
  (add-hook 'skewer-mode-hook
            (lambda () (inferior-js-keys-mode -1))))



(use-package add-node-modules-path
  :after (typescript-mode js-mode js2-mode coffee-mode)
  :config
  (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))


;; ---------------------------------------------------------------------------
;; React.js
;; ---------------------------------------------------------------------------
(use-package rjsx-mode
  :after (flycheck tide)
  :hook
  ((rjsx-mode . (lambda ()
                  (flycheck-add-mode 'jsx-tide 'rjsx-mode)
                  (flycheck-add-mode 'tsx-tide 'rjsx-mode)))
   (rjsx-mode . tide-setup)
   (before-save . tide-format-before-save)))

(use-package rjsx-mode
  :after (tide flycheck)
  :mode ("\\.jsx\\'")
  :config
  (flycheck-select-checker 'tsx-tide))

;;; For typescript
(use-package rjsx-mode
  :after (tide flycheck)
  :mode ("\\.tsx\\'")
  :config
  (flycheck-select-checker 'tsx-tide))

(use-package tide
  :after (eldoc company)
  :hook (((rjsx-mode js2-mode) . tide-setup))
  :commands (tide-setup)
  :config
  (tide-hl-identifier-mode +1)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (company-mode +1))

(use-package emmet-mode
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  :config
  (add-hook 'emmet-mode-hook
            (lambda () (setq emmet-indent-after-insert t))))

(use-package mode-local
  :config
  (setq-mode-local rjsx-mode emmet-expand-jsx-className? t)
  (setq-mode-local web-mode emmet-expand-jsx-className? nil))

(use-package jq-format)

(use-package json-mode)


(provide 'init-javascript)
;;; init-javascript.el ends here
