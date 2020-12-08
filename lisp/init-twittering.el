;;; init-twittering --- Twitter -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package twittering-mode
  :commands twit
  :init
  ;; Avoid to setup password each times
  (setq twittering-use-master-password t)

  :bind (("C-x t" . twit)
         :map twittering-mode-map
         ;; favorite/unfavorite hotkey
         ("C-c f" . twittering-favorite)
         ("C-c F" . twittering-unfavorite)

         ;; rebind hotkey
         ("[" . twittering-switch-to-previous-timeline)
         ("]" . twittering-switch-to-next-timeline)
         ("f" . twittering-other-user-timeline)
         ("b" . twittering-goto-first-status)
         ("n" . twittering-goto-next-status)
         ("p" . twittering-goto-previous-status)
         ("N" . twittering-goto-next-status-of-user)
         ("P" . twittering-goto-previous-status-of-user)
         ;; i-search
         ("s" . isearch-forward))

  :hook (twittering-mode . sanityinc/no-trailing-whitespace)
  :config
  ;; setup status-format
  (twittering-update-status-format "%i %S @%s,  %@:\n%FILL{  %T // from %f%L%r%R}\n"))


(provide 'init-twittering)
;;; init-twittering.el ends here
