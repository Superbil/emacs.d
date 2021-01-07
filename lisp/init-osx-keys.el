;;; init-osx-keys.el --- Configure keys specific to MacOS -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
(setq-default default-input-method "MacOSX")
;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))

(setq mac-pass-command-to-system nil)

(use-package nxml-mode
  :ensure nil
  :bind (:map nxml-mode-map
              ("M-h" . nil)))


(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
