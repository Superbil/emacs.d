;;; init-ruby.el --- Support for the Ruby language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Basic ruby setup
(use-package ruby-hash-syntax)

(use-package conf-mode
  :mode ("Gemfile\\.lock\\'"))

(use-package ruby-mode
  :bind (:map ruby-mode-map
              ("TAB" . indent-for-tab-command)
              ([S-f7] . ruby-compilation-this-buffer)
              ([f7] . ruby-compilation-this-test))
  :ensure nil
  :mode ("Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
         "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
         "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

  :init
  (setq-default ruby-use-encoding-map nil
                ruby-insert-encoding-magic-comment nil)
  :preface
  ;; Customise highlight-symbol to not highlight do/end/class/def etc.
  (defun sanityinc/suppress-ruby-mode-keyword-highlights ()
    "Suppress highlight-symbol for do/end etc."
    (set (make-local-variable 'highlight-symbol-ignore-list)
         (list (concat "\\_<" (regexp-opt '("do" "end")) "\\_>"))))
  :hook
  ((ruby-mode . subword-mode)
   (ruby-mode . sanityinc/suppress-ruby-mode-keyword-highlights))
  :config
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(use-package page-break-lines
  :config
  (push 'ruby-mode page-break-lines-modes))

(use-package rspec-mode)


(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")


;;; Inferior ruby
(use-package inf-ruby)



;;; Ruby compilation
(use-package ruby-compilation
  :config
  (defalias 'rake 'ruby-compilation-rake))


;;; Robe
(use-package robe
  :commands robe-mode
  :after (ruby-mode)
  :hook (ruby-mode . robe-mode)
  :config
  (use-package company
    :after company
    :config
    (push 'company-robe company-backends)))



;;; ri support
(use-package yari
  :config
  (defalias 'ri 'yari))


(use-package bundler)


(use-package yard-mode
  :after yard-mode
  :hook (ruby-mode . yard-mode)
  :diminish yard-mode)


;;; ERB
(use-package mmm-mode
  :after (ruby derived)
  :requires ruby
  :mode ("\\.jst\\.ejs\\'" . html-erb-mode)

  :preface
  (defun sanityinc/set-up-mode-for-erb (mode)
    (add-hook (derived-mode-hook-name mode) (lambda () (require 'mmm-erb)))
    (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

  :config
  (dolist (mode '(html-mode html-erb-mode nxml-mode))
    (sanityinc/set-up-mode-for-erb mode)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))

  (mapc 'sanityinc/set-up-mode-for-erb
        '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

  (add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")

  (mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
  (sanityinc/set-up-mode-for-erb 'yaml-mode)

  (dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
    (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb)))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------

;; Needs to run after rinari to avoid clobbering font-lock-keywords?

;; (require-package 'mmm-mode)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;; (add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)



(use-package rbenv
  :if (executable-find "rbenv"))


(provide 'init-ruby)
;;; init-ruby.el ends here
