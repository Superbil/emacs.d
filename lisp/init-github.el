(use-package yagist
  :config
  (add-hook 'yagist-list-hook 'sanityinc/no-trailing-whitespace))

(use-package bug-reference-github
  :after prog-mode
  :config
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package github-browse-file)
(use-package github-clone)
(use-package github-issues)
(use-package magit-gh-pulls
  :commands magit-status)


(provide 'init-github)
