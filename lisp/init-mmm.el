;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(use-package mmm-mode
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 2))

(provide 'init-mmm)
