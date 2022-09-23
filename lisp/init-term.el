;;; init-term.el --- Support elisp manually installed in the site-lisp dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(use-package term
  :ensure nil
  :hook (term-mode . (lambda ()
                       (setq line-spacing 0))))

(use-package vterm
  :hook (vterm-mode . sanityinc/no-trailing-whitespace)
  :bind (:map vterm-mode-map ("C-y" . vterm-yank))
  :commands vterm)

(provide 'init-term)
;;; init-term.el ends here
