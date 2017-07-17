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
  ;; Use 'go get -u golang.org/x/tools/cmd/goimports' to install
  (setq gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package flymake-go
  :after go-mode)

(use-package go-autocomplete
  :after go-mode)


(provide 'init-go)
