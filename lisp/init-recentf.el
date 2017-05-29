(use-package recentf
  :ensure nil
  :init
  (setq-default recentf-max-saved-items 1000
                recentf-exclude '("/tmp/" "/ssh:"))
  :config
  (add-hook 'after-init-hook (lambda () (recentf-mode 1))))


(provide 'init-recentf)
