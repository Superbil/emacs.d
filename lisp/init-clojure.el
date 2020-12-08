;;; init-clojure.el --- Clojure support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; See also init-clojure-cider.el

(use-package clojure-mode
  :hook
  (clojure-mode . sanityinc/lisp-setup)
  (clojure-mode . subword-mode)
  :config
  (use-package cljsbuild-mode)
  (use-package elein))


(provide 'init-clojure)
;;; init-clojure.el ends here
