(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-Y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :diminish
  :preface
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  (defun sort-dired-buffers (buffers)
    "Sort BUFFERS by moving all Dired buffers to the end."
    (let (dired-buffers other-buffers)
      (dolist (buf buffers)
        (if (with-current-buffer buf
              (eq major-mode 'dired-mode))
            (push buf dired-buffers)
          (push buf other-buffers)))
      (nreverse (append dired-buffers other-buffers))))

  (defun helm-buffers-sort-dired-buffers (orig-fun &rest args)
    (sort-dired-buffers (apply orig-fun args)))

  :functions (sort-dired-buffers helm-buffers-sort-dired-buffers)

  :config
  (setq projectile-completion-system 'helm)
  (advice-add 'helm-buffers-sort-transformer :around #'helm-buffers-sort-dired-buffers)
  (helm-mode 1))

(use-package helm-projectile
  :after projectile-mode
  ;; :bind ("M-t" . helm-projectile-find-file)
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

(use-package helm-gtags
  :if (executable-find "global")
  :disabled (fset 'helm-gtags-mode nil)
  :defer t
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-find-tag)
              ("M-," . helm-gtags-pop-stack)
              ("C-t r" . helm-gtags-find-rtag)
              ("C-t s" . helm-gtags-find-symbol)
              ("C-t p" . helm-gtags-parse-file))
  :hook ((c++-mode . helm-gtags-mode)
         (c-mode . helm-gtags-mode)
         (objc-mode . helm-gtags-mode)
         ;; (python-mode-hook . helm-gtags-mode)
         )
  :diminish (helm-gtags-mode . ("tags"))
  :config
  (global-unset-key "\C-t")
  (custom-set-variables
   '(helm-gtags-path-style 'relative)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update t)
   '(helm-gtags-prefix-key "C-t")))

;; (use-package helm-gitignore)

(use-package helm-tramp)

(provide 'init-helm)
