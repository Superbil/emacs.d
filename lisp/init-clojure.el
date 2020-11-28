;;; init-clojure.el --- Clojure support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See also init-clojure-cider.el

(use-package clojure-mode
  :config
  (use-package cljsbuild-mode)
  (use-package elein)

  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))


(provide 'init-clojure)
;;; init-clojure.el ends here
