(use-package eshell
  :bind (("C-x m" . eshell)
         ;; Change default mail to "C-x M"
         ("C-x M" . compose-mail))
  :ensure nil
  :init
  ;; http://sakito.jp/emacs/emacsshell.html
  (setq system-uses-terminfo nil
        eshell-ls-exclude-regexp "^\\(:2e\\|__MACOSX\\)"
        eshell-visual-subcommands '(("git" "log" "diff" "show")))
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
                     (locate-dominating-file default-directory ".git"))))

    (defun eshell/clear ()
      (interactive)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))
      (eshell-send-input))

    :config
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (bind-key "C-l" 'eshell/clear eshell-mode-map))))
  )


(provide 'init-eshell)
