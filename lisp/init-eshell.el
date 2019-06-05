(use-package eshell
  :bind (("C-x m" . eshell)
         ;; Change default mail to "C-x M"
         ("C-x M" . compose-mail))
  :ensure nil
  :init
  ;; http://sakito.jp/emacs/emacsshell.html
  (setq system-uses-terminfo nil
        eshell-ls-exclude-regexp "^\\(:2e\\|__MACOSX\\)"
        eshell-visual-subcommands '(("git" "log" "diff" "show"))
        eshell-hist-ignoredups t
        eshell-cmpl-cycle-completions nil
        eshell-cmpl-ignore-case t
        eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  :preface

  :config
  ;; use emacs in eshell
  (add-hook 'eshell-mode-hook
            '(lambda nil
               (eshell/export "EDITOR=emacsclient -n")
               (eshell/export "VISUAL=emacsclient -n")))

  (use-package esh-mode
    :ensure nil
    :defer t
    :preface
    (defun eshell/cds ()
      (eshell/cd (or (locate-dominating-file default-directory "src")
                     (locate-dominating-file default-directory ".git")))))
  )

(use-package helm-eshell
  :after eshell
  :ensure nil
  :config
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history))))


(provide 'init-eshell)
