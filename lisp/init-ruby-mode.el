;;; Basic ruby setup
(use-package ruby-mode
  :bind (:map ruby-mode-map
              ("TAB" . indent-for-tab-command))
  :ensure nil
  :mode ("Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
         "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
         "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'"
         "Podfile\\'" "\\.podspec\\'")

  :init
  (setq-default ruby-use-encoding-map nil
                ruby-insert-encoding-magic-comment nil)
  :preface
  ;; Customise highlight-symbol to not highlight do/end/class/def etc.
  (defun sanityinc/suppress-ruby-mode-keyword-highlights ()
    "Suppress highlight-symbol for do/end etc."
    (set (make-local-variable 'highlight-symbol-ignore-list)
         (list (concat "\\_<" (regexp-opt '("do" "end")) "\\_>"))))

  :config
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook))))

  (add-hook 'ruby-mode-hook 'subword-mode)
  (add-hook 'ruby-mode-hook 'sanityinc/suppress-ruby-mode-keyword-highlights))

(use-package page-break-lines
  :config
  (push 'ruby-mode page-break-lines-modes))


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


;;; ri support
(use-package yari
  :config
  (defalias 'ri 'yari))


(use-package goto-gem)


(use-package bundler)


(use-package rvm
  :if (executable-find "rvm")
  :after ruby
  :config
  (rvm-use-default))


(provide 'init-ruby-mode)
