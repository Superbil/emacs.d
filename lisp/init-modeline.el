(use-package telephone-line
  ;; telephone-line-faces
  :config
  (telephone-line-mode))

(use-package all-the-icons)

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
