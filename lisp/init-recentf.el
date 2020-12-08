;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :ensure nil
  :init
  (setq-default recentf-max-saved-items 1000
                recentf-exclude '("/tmp/" "/ssh:"))
  :hook (after-init . recentf-mode))


(provide 'init-recentf)
;;; init-recentf.el ends here
