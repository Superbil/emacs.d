(use-package sgml-mode
  :mode "\\.\\(jsp\\|tmpl\\)\\'")

(use-package tagedit
  :after sgml-mode
  :config
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(use-package web-mode
  :init
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  :mode ("\\.html?\\'" . web-mode))


(provide 'init-html)
