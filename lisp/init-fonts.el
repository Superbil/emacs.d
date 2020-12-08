;;; init-fonts.el --- Custom fonts and scale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Character sets

;;; Changing font sizes
(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease))
  :config
  (add-hook 'after-init-hook 'default-text-scale-mode))


(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  ;; TODO: submit as patch
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)


(provide 'init-fonts)
;;; init-fonts.el ends here
