;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace t)


;;; Whitespace
(use-package whitespace-cleanup-mode
  :diminish
  :preface
  (defun sanityinc/no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq show-trailing-whitespace nil))

  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  (dolist (hook '(special-mode-hook
                  Info-mode-hook
                  eww-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  compilation-mode-hook
                  twittering-mode-hook
                  minibuffer-setup-hook))
    (add-hook hook #'sanityinc/no-trailing-whitespace)))

(use-package simple
  :bind ([remap just-one-space] . cycle-spacing)
  :ensure nil)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
