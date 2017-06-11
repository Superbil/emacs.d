(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Emacs cask seems to depend on the EMACS environment variable being set to the binary path of emacs.
(setenv "EMACS"
        (file-truename (expand-file-name invocation-name invocation-directory)))


(provide 'init-exec-path)
