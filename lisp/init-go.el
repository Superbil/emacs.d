(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(use-package flymake-go
  :after go)

(provide 'init-go)
