(use-package sgml-mode
  :mode "\\.\\(jsp\\|tmpl\\)\\'")

(use-package tagedit
  :after sgml-mode
  :config
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(use-package web-mode
  :pin melpa-stable
  :init
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-ac-sources-alist '(("css" . (ac-source-css-property))
                                    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :mode ("\\.html?\\'" . web-mode))

(use-package htmlize)


(provide 'init-html)
