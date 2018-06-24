(use-package async
  :diminish)

(use-package dired-async
  :ensure nil
  :after async
  :config
  (dired-async-mode 1))


(provide 'init-async)
