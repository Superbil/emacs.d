(use-package tramp
  :ensure nil
  :init
  ;;; Use rsync is quickly or just use ssh
  (setq tramp-default-method "rsync")
  :bind (("C-c s u" . sudo-edit-current-file))
  :config
  (defun prepare-tramp-sudo-string (tempfile)
    (if (version< tramp-version "2.3")
        (if (file-remote-p tempfile)
            (let ((vec (tramp-dissect-file-name tempfile)))
              (tramp-make-tramp-file-name
               "sudo"
               (tramp-file-name-user nil)
               (tramp-file-name-host vec)
               (tramp-file-name-localname vec)
               (format "ssh:%s@%s|"
                       (tramp-file-name-user vec)
                       (tramp-file-name-host vec))))
          (concat "/sudo:root@localhost:" tempfile))
      ;; For new version tramp
      (if (file-remote-p tempfile)
          (let ((vec (tramp-dissect-file-name tempfile)))
            (tramp-make-tramp-file-name
             "sudo"
             ""
             (tramp-file-name-domain vec)
             (tramp-file-name-host vec)
             (tramp-file-name-port vec)
             (tramp-file-name-localname vec)
             (format "ssh:%s@%s|"
                     (tramp-file-name-user vec)
                     (tramp-file-name-host vec))))
        (concat "/sudo:root@localhost:" tempfile))))

  (defun sudo-edit-current-file ()
    (interactive)
    (let ((my-file-name)     ; fill this with the file to open
          (position))        ; if the file is already open save position
      (if (equal major-mode 'dired-mode) ; test if we are in dired-mode
          (progn
            (setq my-file-name (dired-get-file-for-visit))
            (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
        (setq my-file-name (buffer-file-name) ; hopefully anything else is an already opened file
              position (point))
        (find-alternate-file (prepare-tramp-sudo-string my-file-name))
        (goto-char position))))

  (defun tramp-terminator ()
    "Write a shell script to call this emacsclient -e '(tramp-terminator)'"
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))

  ;; Let tramp quicky
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (setq tramp-auto-save-directory "/tmp"))


(use-package helm-tramp
  :hook
  ((helm-tramp-pre-command-hook . helm-tramp/disable-config)
   (helm-tramp-quit-hook . helm-tramp/enable-config))
  :preface
  (defun helm-tramp/disable-config ()
    (global-aggressive-indent-mode 0)
    (projectile-mode 0)
    (editorconfig-mode 0))
  (defun helm-tramp/enable-config ()
    (global-aggressive-indent-mode 1)
    (projectile-mode 1)
    (editorconfig-mode 1)))


(provide 'init-tramp)
