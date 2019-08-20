(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 4)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
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
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))

  :config
  ;; ... but enable it if flycheck can't handle javascript
  (use-package flycheck
    :after flycheck-get-checker-for-buffer
    :config
    (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active))

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (js2-imenu-extras-setup))

;; js-mode
(use-package js-mode
  :ensure nil
  :mode ("node" . preferred-javascript-mode)
  :init
  (setq-default js-indent-level preferred-javascript-indent-level))


;; Javascript nests {} and () a lot, so I find this helpful

(use-package xref-js2
  :bind (:map js2-mode-map
              ("M-." . nil))
  :after js2-mode
  :if (executable-find "ag")
  :config
  (add-hook 'js2-mode-hook
            (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))



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
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


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


;; ---------------------------------------------------------------------------
;; React.js
;; ---------------------------------------------------------------------------
(use-package rjsx-mode
  :after (flycheck tide)
  :mode ("\\.jsx\\'")
  :preface
  (defun jsx-tide-4-rjsx-mode ()
    (flycheck-add-mode 'jsx-tide 'rjsx-mode)
    (flycheck-select-checker 'jsx-tide))
  :hook ((rjsx-mode . jsx-tide-4-rjsx-mode)))

;;; For typescript
(use-package rjsx-mode
  :after (flycheck tide)
  :mode ("\\.tsx\\'")
  :preface
  (defun tsx-tide-4-rjsx-mode ()
    (flycheck-add-mode 'tsx-tide 'rjsx-mode)
    (flycheck-select-checker 'tsx-tide))
  :hook ((rjsx-mode . tsx-tide-4-rjsx-mode)))

(use-package tide
  :after (eldoc company)
  :hook (((rjsx-mode js2-mode) . tide-setup))
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


;;; Use Prettier to format code


(provide 'init-javascript)
