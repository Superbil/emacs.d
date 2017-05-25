;;; Basic ruby setup
(use-package ruby-mode
  :bind (:map ruby-mode-map
              ("TAB" . indent-for-tab-command))

  :mode ("Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
         "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
         "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'"
         "Podfile\\'" "\\.podspec\\'")

  :init
  (setq-default ruby-use-encoding-map nil
                ruby-insert-encoding-magic-comment nil)
  :config
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook))))

  (add-hook 'ruby-mode-hook 'subword-mode))

(use-package ruby-hash-syntax)

(use-package conf-mode
  :mode "Gemfile\\.lock\\'")

(use-package page-break-lines
  :config
  (push 'ruby-mode page-break-lines-modes))

(use-package rspec-mode)


;;; Inferior ruby
(use-package inf-ruby)


;;; Ruby compilation
(use-package ruby-compilation
  :after ruby-mode
  :bind (:map ruby-mode-map
              ([S-f7] . ruby-compilation-this-buffer)
              ([f7] . ruby-compilation-this-test))
  :config
  (defalias 'rake 'ruby-compilation-rake))


;;; Robe
(use-package robe
  :after ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)

  (use-package company
    :config
    (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook html-erb-mode-hook haml-mode))
      (add-hook hook
                (lambda () (sanityinc/local-push-company-backend 'company-robe))))))


(use-package ruby-mode
  :preface
  ;; Customise highlight-symbol to not highlight do/end/class/def etc.
  (defun sanityinc/suppress-ruby-mode-keyword-highlights ()
    "Suppress highlight-symbol for do/end etc."
    (set (make-local-variable 'highlight-symbol-ignore-list)
         (list (concat "\\_<" (regexp-opt '("do" "end")) "\\_>"))))
  :config
  (add-hook 'ruby-mode-hook 'sanityinc/suppress-ruby-mode-keyword-highlights))


;;; ri support
(use-package yari
  :config
  (defalias 'ri 'yari))


(use-package goto-gem)


(use-package bundler)


;;; ERB
(use-package mmm-mode
  :preface
  (defun sanityinc/ensure-mmm-erb-loaded ()
    (require 'mmm-erb))
  :mode ("\\.jst\\.ejs\\'"  . html-erb-mode)
  :config
  (use-package derived
    :preface
    (defun sanityinc/set-up-mode-for-erb (mode)
      (add-hook (derived-mode-hook-name mode) 'sanityinc/ensure-mmm-erb-loaded)
      (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))
    :config
    (mapc 'sanityinc/set-up-mode-for-erb
          '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))
    (let ((html-erb-modes '(html-mode html-erb-mode nxml-mode)))
      (dolist (mode html-erb-modes)
        (sanityinc/set-up-mode-for-erb mode)
        (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
        (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))))

  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
  (add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
  (mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
  (dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
    (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb)))


(use-package rvm
  :if (executable-find "rvm")
  :after ruby
  :config
  (rvm-use-default))


(provide 'init-ruby-mode)
