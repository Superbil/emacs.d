(use-package gnus
  :init
  (setq gnus-init-file (locate-user-emacs-file "gnus/.gnus.el"))
  (setq gnus-home-directory (locate-user-emacs-file "gnus"))
  (let ((mail-directory (expand-file-name "Mail/" gnus-home-directory)))
    (setq message-directory mail-directory
          nnml-directory mail-directory))
  :preface
  (defun gnus-treat-simplified-chinese ()
    "Convert Simplified Chinese to Traditional Chinese using opencc."
    (interactive)
    (gnus-with-article-buffer
     (article-goto-body)
     (let ((opencc (executable-find "opencc")))
       (when opencc
         (shell-command-on-region (point) (point-max) opencc t t)))))

  (defun my-gnus-group-exit (&optional really-exit)
    (interactive "P")
    (if really-exit
        (gnus-group-exit)
      (bury-buffer)))
  :bind (:map gnus-group-mode-map ("q" . my-gnus-group-exit)))

(use-package mailcap
  :if *is-a-mac*
  :config
  (mailcap-add-mailcap-entry "application" "pdf" '((viewer "/usr/bin/qlmanage -p %s") (type . "application/pdf")))
  (mailcap-add-mailcap-entry "image" "jpeg" '((viewer "/usr/bin/qlmanage -p %s") (type . "image/*"))))

(provide 'init-gnus)
