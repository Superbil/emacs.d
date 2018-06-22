;;; Character sets


(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  ;; TODO: submit as patch
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)

;;; Setup chinese font and setup font
(use-package cnfonts
  :bind (("C-M-=" . cnfonts-increase-fontsize)
         ("C-M--" . cnfonts-decrease-fontsize))
  :init
  (setq cnfonts-use-face-font-rescale t)
  (let ((fonts
         '(("Source Code Pro" "Monaco" "Consolas")
           ("Source Han Sans TC" "Source Han Sans")
           ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB"))))
    (setq cnfonts-personal-fontnames fonts)
    (setq cnfonts--custom-set-fontnames fonts))
  (setq cnfonts--custom-set-fontsizes
        '((9    10.5 10.5)
          (10   12.0 12.0)
          (11.5 15.0 12.5)
          (12.5 15.0 16.0)
          (14   16.5 16.5)
          (15   18.0 18.0)
          (16   20.0 19.5)
          (18   22.0 21.0)
          (20   24.0 24.0)
          (22   26.0 25.5)
          (24   28.5 28.5)
          (26   32.0 31.5)
          (28   34.0 33.0)
          (30   36.0 36.0)
          (32   38.5 39.0)))
  :config
  (cnfonts-enable))

(provide 'init-fonts)
