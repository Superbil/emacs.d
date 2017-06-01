(use-package swimmers
  :commands (swimming swimming-in-sea)
  :ensure nil
  :pin manual
  :config
  (add-hook 'swimmers-begin-hook #'sanityinc/no-trailing-whitespace))


(provide 'init-games)
