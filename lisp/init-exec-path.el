;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell)

(after-load 'exec-path-from-shell
            (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "GOPATH"))
              (add-to-list 'exec-path-from-shell-variables var)))

;; Emacs cask seems to depend on the EMACS environment variable being set to the binary path of emacs.
(setenv "EMACS"
        (file-truename (expand-file-name invocation-name invocation-directory)))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
