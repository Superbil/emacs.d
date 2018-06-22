(use-package telephone-line
  ;; telephone-line-faces
  :config
  (telephone-line-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


(provide 'init-modeline)
