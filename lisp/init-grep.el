(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(use-package ag
  :bind (("M-?" . ag-project))
  :if (executable-find "ag")
  :config
  (setq-default ag-highlight-search t))

(use-package wgrep
  :pin melpa)

(use-package wgrep-ag
  :pin melpa
  :after wgep
  :if (executable-find "ag"))


(provide 'init-grep)
