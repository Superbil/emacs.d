(use-package projectile
  :bind (:map projectile-command-map
              ("P" . projectile-switch-open-vc))
  :after (text-mode prog-mode)
  :init
  (setq projectile-verbose nil
        projectile-completion-system 'ivy)
  :preface
  (defun projectile-switch-open-vc (&optional arg)
    "Switch to a project we have currently opened.
      Invokes `projectile-vc' after switch to project"
    (interactive "P")
    (-if-let* ((projects (projectile-relevant-open-projects))
               (projectile-switch-project-action 'projectile-vc))
        (projectile-switch-project-by-name
         (projectile-completing-read "Switch to open project vc: " projects)
         arg)
      (error "There are no open projects")))

  (defun projectile-xcode-workspace ()
    "Open workspace with Xcode at the root of the project."
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (open-xcode-workspace (projectile-project-root))))
  :config
  (setq projectile-globally-ignored-files (append projectile-globally-ignored-files '(".DS_Store" ".gitignore")))

  (fullframe projectile-switch-open-vc magit-mode-quit-window)

  (after-load 'guide-key
    (dolist (keys `("C-c p" "C-c p 4" "C-c p s" "C-c p x"))
      (add-to-list 'guide-key/guide-key-sequence keys)))

  (add-hook 'after-init-hook
            (lambda ()
              (projectile-cleanup-known-projects))))


(provide 'init-projectile)
