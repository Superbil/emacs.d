(use-package swimmers
  :commands (swimming swimming-in-sea)
  :ensure nil
  :pin manual
  :config
  (add-hook 'swimmers-begin-hook #'sanityinc/no-trailing-whitespace))

(use-package xkcd
  :bind (:map xkcd-mode-map
              ("p" . xkcd-prev)
              ("n" . xkcd-next)))


(provide 'init-games)
