(use-package telephone-line
  :config
  (telephone-line-mode t))
(use-package all-the-icons
  :after telephone-line
  :config
  )

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  ;; only set transformers
  ;; (ivy-set-display-transformer 'ivy-switch-buffer 'all-the-icons-ivy-buffer-transformer)
  )

(provide 'init-modeline)
