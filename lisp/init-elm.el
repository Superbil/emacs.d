(use-package elm-mode
  :diminish elm-indent-mode
  :config
  (when (executable-find "elm-oracle")
    (add-hook 'elm-mode-hook 'elm-oracle-setup-completion))
  (when (executable-find "elm-format")
    (setq-default elm-format-on-save t)))

(use-package flycheck-elm
  :after elm-mode
  :config
  (flycheck-elm-setup))


(provide 'init-elm)
