(use-package desktop
  :ensure nil
  :init
  ;; save a list of open files in ~/.emacs.d/.emacs.desktop
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)
  :config
  (defadvice desktop-read (around time-restore activate)
    (let ((start-time (current-time)))
      (prog1
          ad-do-it
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))

  (defadvice desktop-create-buffer (around time-create activate)
    (let ((start-time (current-time))
          (filename (ad-get-arg 1)))
      (prog1
          ad-do-it
        (message "Desktop: %.2fms to restore %s"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)
                 (when filename
                   (abbreviate-file-name filename))))))

  (desktop-save-mode 1))

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(use-package savehist
  :ensure nil
  :init
  (setq-default history-length 1000)
  :config
  (savehist-mode t))

(use-package session
  :init
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  :config
  (add-hook 'after-init-hook 'session-initialize))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((comint-input-ring        . 50)
                (compile-history          . 30)
                desktop-missing-file-warning
                (dired-regexp-history     . 20)
                (extended-command-history . 30)
                (face-name-history        . 20)
                (file-name-history        . 100)
                (grep-find-history        . 30)
                (grep-history             . 30)
                (ido-buffer-history       . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (ivy-history              . 100)
                (magit-read-rev-history   . 50)
                (minibuffer-history       . 50)
                (org-clock-history        . 50)
                (org-refile-history       . 50)
                (org-tags-history         . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                register-alist
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                tags-table-list)))

;;----------------------------------------------------------------------------
;; Auto cleanup older buffers
;;----------------------------------------------------------------------------
(use-package midnight
  :init
  (setq midnight-delay 0))


(provide 'init-sessions)
