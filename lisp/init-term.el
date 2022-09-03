;;; init-term.el --- Support elisp manually installed in the site-lisp dir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(use-package term
  :ensure nil
  :hook (term-mode . (lambda ()
                       (setq line-spacing 0))))


(provide 'init-term)
;;; init-term.el ends here
