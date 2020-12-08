;;; init-clojure-cider.el --- Cider support for clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cider
  :init
  (setq nrepl-popup-stacktraces nil)
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . subword-mode)
   (cider-repl-mode . paredit-mode))
  :config
  ;; nrepl isn't based on comint
  (add-hook 'cider-repl-mode-hook 'sanityinc/no-trailing-whitespace)

  (use-package flycheck-clojure
    :after (clojure-mode flycheck)
    :config
    (flycheck-clojure-setup)))


(provide 'init-clojure-cider)
;;; init-clojure-cider.el ends here
