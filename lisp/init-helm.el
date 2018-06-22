(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-Y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1)))

(use-package helm-projectile
  :after (projectile-mode)
  :bind ("M-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(use-package helm-ag
  :after helm
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package helm-ls-git)

(use-package helm-github-stars
  :init
  (setq helm-github-stars-username "superbil")
  (setq helm-github-stars-cache-file (expand-file-name "~/Library/Caches/hgs-cache")))


(provide 'init-helm)
