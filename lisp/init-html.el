(use-package sgml-mode
  :mode "\\.\\(jsp\\|tmpl\\)\\'")

(use-package tagedit
  :after sgml-mode
  :config
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

;; Note: ERB is configured in init-ruby-mode


(provide 'init-html)
