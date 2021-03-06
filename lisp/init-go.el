;;; init-go.el --- Go language  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; must install those tootls
;;; go get -u golang.org/x/tools/cmd/...
;;; go get -u github.com/rogpeppe/godef/...
;;; go get -u github.com/nsf/gocode
;;; go get -u golang.org/x/tools/cmd/goimports
;;; go get -u golang.org/x/tools/cmd/guru
;;; go get -u github.com/dougm/goflymake

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c C-k" . godoc)
              ("M-." . godef-jump)
              ("M-]" . next-error)
              ("M-[" . previous-error))
  :init
  (when (locate-file "goimports" exec-path)
    (setq gofmt-command "goimports"))
  :hook (before-save . gofmt-before-save))

(use-package flymake-go
  :after go-mode)

(use-package go-autocomplete
  :after go-mode)

(use-package go-guru
  :if (locate-file "guru" exec-path)
  :after go-mode
  :hook (go-mode . go-guru-hl-identifier-mode))


(provide 'init-go)
;;; init-go.el ends here
