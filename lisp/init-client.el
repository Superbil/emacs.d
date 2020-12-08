;;; init-client.el --- Edit server for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package edit-server
  :commands edit-server-start
  :config
  ;; kill this buffer, also can kill client
  (add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
              (when server-buffer-clients
                (local-set-key (kbd "C-x k") 'server-edit)))))


(provide 'init-client)
;;; init-client.el ends here
