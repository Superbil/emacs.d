(use-package recentf
  :ensure nil
  :init
  (setq-default recentf-max-saved-items 1000
                recentf-exclude '("/tmp/" "/ssh:"))
  :config
  (add-hook 'after-init-hook 'recentf-mode))


(provide 'init-recentf)
