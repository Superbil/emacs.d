;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO: https://wunki.org/posts/2014-05-17-haskell-packages-development.html
;; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el
;; TODO: ghci-ng
;; TODO: don't pop up *Warnings* if haskell-stylish-on-save fails
;; TODO: purescript-mode
(use-package haskell-mode
  :mode "\\.ghci\\'"
  :init
  (setq-default haskell-stylish-on-save t)

  ;; Indentation
  :hook
  ((haskell-mode . turn-on-haskell-indentation)
   (haskell-mode . haskell-auto-insert-module-template))

  :config
  (with-eval-after-load 'haskell
    (bind-key "M-N" 'haskell-goto-next-error interactive-haskell-mode-map)
    (bind-key "M-P" 'haskell-goto-prev-error interactive-haskell-mode-map)))


;; Use intero for completion and flycheck

(use-package intero
  :after (haskell-mode eldoc-mode)
  :hook (haskell-mode . eldoc-mode)
  :config
  (intro-global-mode)
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'intero
                               '(warning . haskell-hlint))))



;; Source code helpers

(use-package hindent
  :after haskell-mode
  :hook (haskell-mode . hindent-mode))

(use-package hayoo
  :after haskell-mode
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle)
              ("C-o" . open-line)))


(use-package page-break-lines
  :after haskell-mode
  :config (push 'haskell-mode page-break-lines-modes))


(provide 'init-haskell)
;;; init-haskell.el ends here
