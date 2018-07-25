;;; Basic ruby setup
(use-package ruby-mode
  :bind (:map ruby-mode-map
              ("TAB" . indent-for-tab-command))
  :ensure nil
  :mode ("Rakefile\\'"
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



(use-package rbenv
  :if (executable-find "rbenv"))

(provide 'init-ruby-mode)
