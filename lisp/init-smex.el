;; Use smex to handle M-x
(use-package smex
  :bind ([remap execute-extended-command] . smex)
  :init
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))


(provide 'init-smex)
