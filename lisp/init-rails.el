(use-package rinari
  :diminish (rinari-minor-mode . "Rin")
  :preface
  (defun update-rails-ctags ()
    (interactive)
    (let ((default-directory (or (rinari-root) default-directory)))
      (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))
  :config
  (global-rinari-mode))

(use-package projectile-rails
  :after projectile-mode
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on)

  (after-load 'guide-key
    (add-to-list 'guide-key/guide-key-sequence "C-c r")))


(provide 'init-rails)
