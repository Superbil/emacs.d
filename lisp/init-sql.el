(use-package sql-indent)

(use-package sql
  :bind (:map sql-mode-map
              ("C-c C-z" . sanityinc/pop-to-sqli-buffer))
  :init
  (setq-default sql-input-ring-file-name
                (expand-file-name ".sqli_history" user-emacs-directory))
  :preface
  (defun sanityinc/pop-to-sqli-buffer ()
    "Switch to the corresponding sqli buffer."
    (interactive)
    (if sql-buffer
        (progn
          (pop-to-buffer sql-buffer)
          (goto-char (point-max)))
      (sql-set-sqli-buffer)
      (when sql-buffer
        (sanityinc/pop-to-sqli-buffer))))

  ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
  (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
    (unless (eq 'oracle sql-product)
      (sql-product-font-lock nil nil)))
  :config
  ;; sql-mode pretty much requires your psql to be uncustomised from stock settings
  (push "--no-psqlrc" sql-postgres-options)

  (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)

  (use-package dash-at-point
    :preface
    (defun sanityinc/maybe-set-dash-db-docset ()
      (when (eq sql-product 'postgres)
        (set (make-local-variable 'dash-at-point-docset) "psql")))
    :config
    (add-hook 'sql-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (defadvice sql-set-product (after set-dash-docset activate)
      (sanityinc/maybe-set-dash-db-docset))))

(use-package page-break-lines
  :after sql-mode
  :config
  (push 'sql-mode page-break-lines-modes))


(provide 'init-sql)
