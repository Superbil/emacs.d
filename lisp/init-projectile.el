;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("P" . projectile-switch-open-vc))
  :preface
  (defun projectile-switch-open-vc (&optional arg)
    "Switch to a project we have currently opened.
      Invokes `projectile-vc' after switch to project"
    (interactive "P")
    (let* ((projects (projectile-relevant-open-projects))
           (projectile-switch-project-action 'projectile-vc)
           (projectile-name (projectile-completing-read "Switch to open project vc: " projects)))
      (when projectile-name
        (projectile-switch-project-by-name projectile-name arg))))

  (defun projectile-xcode-workspace ()
    "Open workspace with Xcode at the root of the project."
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (open-xcode-workspace (projectile-project-root))))

  :config
  ;; Auto-discovery is slow to do by default. Better to update the list
  ;; when you need to (`projectile-discover-projects-in-search-path').
  (setq projectile-auto-discover nil
        projectile-mode-line-prefix " 🎛️"
        projectile-globally-ignored-files (append projectile-globally-ignored-files '(".DS_Store" ".gitignore"))
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o"))


  (fullframe projectile-switch-open-vc magit-mode-quit-window)


  (add-hook 'after-init-hook
            (lambda ()
              (projectile-mode)
              (projectile-cleanup-known-projects))))


(provide 'init-projectile)
;;; init-projectile.el ends here
