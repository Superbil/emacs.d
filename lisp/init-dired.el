;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

(defcustom dired-open-external-program
  "open"
  "Name for dired to use."
  :type 'string)

(use-package dired
  :commands (dired find-name-dired find-dired)
  :ensure nil
  :defer t
  :preface
  (setq-default diredp-hide-details-initially-flag nil
                dired-dwim-target t)
  :config
  (defun image-dired-show-metadata (file-list)
    "Display metatata of buffer image file or marked files in dired.
 (typically image files)"
    (interactive
     (let ((myFileList
            (cond
             ((string-equal major-mode "dired-mode") (dired-get-marked-files))
             ((string-equal major-mode "image-mode") (list (buffer-file-name)))
             (t (list (read-from-minibuffer "file name:"))))))
       (list myFileList)))
    (mapc
     (lambda (ξf)
       (let* ((image-exif "exiftool")
              (cmdStr (format "%s '%s'" image-exif (file-relative-name ξf))))
         (if (shell-command (format "which %s" image-exif))
             (message (format "%s is not installed" image-exif))
           (shell-command cmdStr)
           )))
     file-list))

  (when (fboundp 'toggle-diredp-find-file-reuse-dir)
    (toggle-diredp-find-file-reuse-dir 1)))

;; (use-package dired+
;;   :bind (:map dired-mode-map
;;               ("/" . dired-show-only)
;;               ([mouse-2] . dired-find-file)
;;               ("C-c C-j" . dired-open-current-directory)
;;               ("C-c C-o" . dired-open-file))
;;   :commands toggle-diredp-find-file-reuse-dir
;;   :preface
;;   (defun dired-show-only (regexp)
;;     (interactive "sFiles to show (regexp): ")
;;     (dired-mark-files-regexp regexp)
;;     (dired-toggle-marks)
;;     (dired-do-kill-lines))

;;   (defun dired-open-current-directory ()
;;     "Open current directory with external program."
;;     (interactive)
;;     (call-process dired-open-external-program nil 0 nil (file-truename default-directory)))

;;   (defun dired-open-file ()
;;     "In dired, open the file named on this line with external program."
;;     (interactive)
;;     (let* ((file (dired-get-file-for-visit)))
;;       (call-process dired-open-external-program nil 0 nil file)))

;;   (defun dired-show-image-exif ()
;;     "Show image EXIF metadata using exiftool."
;;     (interactive)
;;     (let* ((exiftool (executable-find "exiftool"))
;;            (file (dired-get-file-for-visit)))
;;       (when exiftool
;;         (shell-command (format "exiftool %s" file) "*Image EXIF*" nil))))

;;   :config
;;   (when (fboundp 'global-dired-hide-details-mode)
;;     (global-dired-hide-details-mode -1))
;;   (setq dired-recursive-deletes 'top)


(use-package diff-hl
  :after dired
  :hook (dired-mode . diff-hl-dired-mode))

(use-package dired-x
  :if *is-a-mac*
  :after dired
  :ensure nil
  :config
  (setq dired-guess-shell-alist-user
        '(("\\.xcodeproj$" "open")
          ("\\.xcworkspace" "open")
          ("\\.pdf" "open"))))


(provide 'init-dired)
;;; init-dired.el ends here
