(use-package sane-term
  :commands sane-term
  :bind (("<f10>" . sane-term))
  :config
  ;;; term use zsh
  (setq-when-file-existed sane-term-shell-command (executable-find "zsh")))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(use-package term
  :ensure nil
  :hook (term-mode . (lambda ()
                       (setq line-spacing 0))))


(provide 'init-term)
