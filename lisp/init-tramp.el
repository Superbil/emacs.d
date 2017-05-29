(use-package tramp
  :ensure nil
  :init
  ;;; Use rsync is quickly or just use ssh
  (setq tramp-default-method "ssh")
  :bind (("C-c s u" . sudo-edit-current-file))
  :config
  (defun prepare-tramp-sudo-string (tempfile)
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
      (concat "/sudo:root@localhost:" tempfile)))

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
    (tramp-cleanup-all-buffers)))


(provide 'init-tramp)
