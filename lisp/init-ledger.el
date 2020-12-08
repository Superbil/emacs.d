;;; init-ledger.el --- Support for the ledger CLI accounting tool -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ledger-mode
  :bind (:map ledger-mode-map
              ("RET" . newline)
              ("C-o" . open-line))
  :mode ("\\.ledger$" . ledger-mode)
  :after (flycheck ledger-mode)
  :init
  (setq ledger-highlight-xact-under-point nil
        ledger-use-iso-dates nil)
  :hook (ledger-mode . goto-address-prog-mode)
  :config
  (use-package flycheck-ledger
    :after ledger-mode
    :config
    (require 'flycheck-ledger))

  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :config
    (exec-path-from-shell-copy-env "LEDGER_FILE")))


(provide 'init-ledger)
;;; init-ledger.el ends here
