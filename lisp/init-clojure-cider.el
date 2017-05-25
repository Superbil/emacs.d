(use-package cider
  :init
  (setq nrepl-popup-stacktraces nil)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook 'sanityinc/no-trailing-whitespace)

  (use-package flycheck-clojure
    :after (clojure-mode flycheck)
    :config
    (flycheck-clojure-setup)))


(provide 'init-clojure-cider)
