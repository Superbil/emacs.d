;;; init-osx.el --- Setup for macOSX -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun superbil/osx-open-app (app &optional app-args)
  "Use OSX open `APP' with APP-ARGS or not."
  (shell-command
   (if app-args
       (format (concat "open -a " app " %s")
               (shell-quote-argument app-args))
     (format (concat "open -a " app)))))

(defun superbil/osx-open (&optional path)
  "Use OSX open `app' with PATH."
  (interactive)
  (let ((process-connection-type nil)
        (open-path (if path path ".")))
    (start-process "osx-open" nil "open" open-path)))

(use-package f
  :ensure nil
  :config
  (defun open-xcode-workspace (path)
    "Open workspace with Xcode at `PATH'."
    (interactive)
    (let* ((workspace (f-glob "*.xcworkspace" path))
           (workspace (or workspace (f-glob "**/*.xcworkspace" path)))
           (l (length workspace)))
      (when (= l 0) (user-error "No found workspace to work"))
      (when (> l 1) (user-error "Too many workspace %s" workspace))
      (superbil/osx-open (car workspace)))))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :init
  (setq delete-by-moving-to-trash t))


(provide 'init-osx)
;;; init-osx.el ends here
