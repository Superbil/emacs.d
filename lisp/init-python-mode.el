(use-package python-mode
  :ensure nil
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)))

;; python side must install elpy rope pyflakes pep8 [jedi]
(use-package elpy
  :after python-mode
  :config (elpy-enable))

;;; Setup elpy use ipython
(use-package elpy
  :if (executable-find "jupyter")
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  ;; Emacs IPython Notebook
  (use-package ein))

(use-package pip-requirements)

(use-package anaconda-mode
  :after python-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (use-package company-anaconda
    :after company
    :config
    (add-hook 'python-mode-hook
              (lambda () (sanityinc/local-push-company-backend 'company-anaconda)))))

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(use-package flymake-python-pyflakes
  :after python-mode
  :config
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))


;;----------------------------------------------------------------------------
;; pdbtrack constants
;;----------------------------------------------------------------------------
(defconst py-pdbtrack-stack-entry-regexp
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")


(provide 'init-python-mode)
