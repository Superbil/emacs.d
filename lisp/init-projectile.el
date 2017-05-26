(use-package projectile
  :bind (:map projectile-command-map
              ("P" . projectile-switch-open-vc))
  :after (text-mode prog-mode)
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
  (fullframe projectile-switch-open-vc magit-mode-quit-window)

  (after-load 'magit
    (add-hook 'magit-status-mode-hook 'projectile-mode))
  (after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c p")))


(provide 'init-projectile)
