(use-package browse-at-remote)
(use-package yagist
  :config
  (add-hook 'yagist-list-hook 'sanityinc/no-trailing-whitespace))

(use-package magit-gh-pulls
  :commands magit-status)


(provide 'init-github)
