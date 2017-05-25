(use-package ledger-mode
  :bind (:map ledger-mode-map
              ("RET" . newline)
              ("C-o" . open-line))
  :mode ("\\.ledger$" . ledger-mode)
  :after (flycheck ledger-mode)
  :init
  (setq ledger-highlight-xact-under-point nil
        ledger-use-iso-dates nil)
  :config
  (use-package flycheck-ledger)

  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "LEDGER_FILE"))

  (add-hook 'ledger-mode-hook 'goto-address-prog-mode))


(provide 'init-ledger)
