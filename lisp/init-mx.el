;; Use amx to handle M-x
(use-package amx
  :init
  (setq-default amx-save-file (expand-file-name ".amx-items" user-emacs-directory))
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (amx-mode))))


(provide 'init-mx)
