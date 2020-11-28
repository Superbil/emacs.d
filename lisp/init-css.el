;;; init-css.el --- CSS/Less/SASS/SCSS support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Colourise CSS colour literals
(use-package rainbow-mode
  :config
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))


;;; Embedding in html
(use-package mmm-mode
  :after mmm-var
  :config
  (mmm-add-group
   'html-css
   '((css-cdata
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
      :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
      :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t]*\n?"
      :back "[ \t]*</style>"
      :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css-inline
      :submode css-mode
      :face mmm-code-submode-face
      :front "style=\""
      :back "\"")))
  (dolist (mode (list 'html-mode 'nxml-mode))
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))




;;; SASS and SCSS
(use-package sass-mode)
(use-package scss-mode
  :init
  (setq-default scss-compile-at-save nil))


;;; LESS
(use-package less-css-mode
  :config
  (use-package skewer-less
    :config
    (add-hook 'less-css-mode-hook 'skewer-less-mode)))


;; Skewer CSS
(use-package skewer-mode
  :after css-mode
  :config
  (add-hook 'css-mode-hook 'skewer-css-mode))


;;; Use eldoc for syntax hints
(use-package css-eldoc
  :after css-mode
  :config
  (autoload 'turn-on-css-eldoc "css-eldoc")
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))


(provide 'init-css)
;;; init-css.el ends here
