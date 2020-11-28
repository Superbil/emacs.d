;;; init-mmm.el --- Multiple Major Modes support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(use-package mmm-mode
  :after mmm-auto
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 2))


(provide 'init-mmm)
;;; init-mmm.el ends here
