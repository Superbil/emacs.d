;;; init-async.el --- Async packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package async
  :diminish)

(use-package dired-async
  :ensure nil
  :after async
  :config
  (dired-async-mode 1))


(provide 'init-async)
;;; init-async.el ends here
