;;; init-dash.el --- Integrate with the Mac app "Dash" -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Support for the http://kapeli.com/dash documentation browser

(use-package dash
  :config (dash-enable-font-lock))

(use-package dash-at-point
  :bind  ("C-c D" . dash-at-point)
  :preface
  (defun dash-at-point-installed-docsets ()
    (let ((dash-defaults (shell-command-to-string "defaults read com.kapeli.dashdoc docsets"))
          (keyword-regexp (rx (or "platform" "pluginKeyword") space "=" space (group (1+ word)) ";\n")))
      (-distinct (cl-map 'list #'cdr (s-match-strings-all keyword-regexp dash-defaults)))))
  :config
  (setq dash-at-point-docsets (or (dash-at-point-installed-docsets) dash-at-point-docsets)))


(provide 'init-dash)
;;; init-dash.el ends here
