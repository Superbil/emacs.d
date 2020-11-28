;;; init-github.el --- Github integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package browse-at-remote)
(use-package yagist
  :commands yagist-list
  :init
  (setq yagist-encrypt-risky-config t
        yagist-git-config-with-includes t)
  :config
  (add-hook 'yagist-list-hook 'sanityinc/no-trailing-whitespace))

(use-package magit-gh-pulls
  :commands magit-status)


(provide 'init-github)
;;; init-github.el ends here
