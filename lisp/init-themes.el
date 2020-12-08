;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

(use-package dracula-theme)
(use-package color-theme-sanityinc-solarized)
(use-package color-theme-sanityinc-tomorrow)


(provide 'init-themes)
;;; init-themes.el ends here
