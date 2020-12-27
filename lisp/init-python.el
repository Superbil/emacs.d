;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure nil
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)))

;;; Setup elpy
(use-package elpy)

;; Emacs IPython Notebook
(use-package ein
  :commands (ein:notebooklist-open))

(use-package pip-requirements)

(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :after (company python-mode)
    :config
    (push 'company-anaconda company-backends))
  ;; Fix some reading error https://github.com/pythonic-emacs/anaconda-mode#faq
  (when *is-a-mac*
    (setq anaconda-mode-localhost-address "localhost"))

  (setq mahmoudimus/patched-container-name "app")

  (use-package el-patch
    :config
    (el-patch-feature anaconda-mode)
    (el-patch-defun anaconda-mode-bootstrap-filter (process output &optional callback)
      "Set `anaconda-mode-port' from PROCESS OUTPUT.
Connect to the `anaconda-mode' server.  CALLBACK function will be
called when `anaconda-mode-port' will be bound."
      ;; Mimic default filter.
      (when (buffer-live-p (process-buffer process))
        (with-current-buffer (process-buffer process)
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))))
      (--when-let (s-match "anaconda_mode port \\([0-9]+\\)" output)
        (setq anaconda-mode-port (string-to-number (cadr it)))
        (set-process-filter process nil)
        (cond ((pythonic-remote-docker-p)
               (let* ((container-raw-description (with-output-to-string
                                                   (with-current-buffer
                                                       standard-output
                                                     (call-process "docker" nil t nil "inspect" (pythonic-remote-host)))))
                      (container-description (let ((json-array-type 'list))
                                               (json-read-from-string container-raw-description)))
                      (container-ip (cdr (assoc 'IPAddress
                                                (cdadr (assoc 'Networks
                                                              (cdr (assoc 'NetworkSettings
                                                                          (car container-description)))))))))
                 (setq anaconda-mode-socat-process
                       (start-process anaconda-mode-socat-process-name
                                      anaconda-mode-socat-process-buffer
                                      "socat"
                                      (format
                                       (el-patch-swap
                                         "TCP4-LISTEN:%d"
                                         "TCP4-LISTEN:%d,reuseaddr,fork")
                                       anaconda-mode-port)
                                      (format
                                       (el-patch-swap
                                         "TCP4:%s:%d"
                                         "EXEC:docker-compose exec -T %s 'socat STDIO TCP-CONNECT:localhost:%d'")
                                       (el-patch-swap container-ip mahmoudimus/patched-container-name)
                                       anaconda-mode-port)))
                 (set-process-query-on-exit-flag anaconda-mode-socat-process nil)))
              ((pythonic-remote-vagrant-p)
               (setq anaconda-mode-ssh-process
                     (start-process anaconda-mode-ssh-process-name
                                    anaconda-mode-ssh-process-buffer
                                    "ssh" "-nNT"
                                    (format "%s@%s" (pythonic-remote-user) (pythonic-remote-host))
                                    "-p" (number-to-string (pythonic-remote-port))
                                    "-L" (format "%s:%s:%s" anaconda-mode-port (pythonic-remote-host) anaconda-mode-port)))
               (set-process-query-on-exit-flag anaconda-mode-ssh-process nil)))
        (when callback
          (funcall callback)))))
  )

(use-package pythonic)

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(use-package flymake-python-pyflakes
  :after python-mode
  :hook (python-mode . flymake-python-pyflakes-load))


;;----------------------------------------------------------------------------
;; pdbtrack constants
;;----------------------------------------------------------------------------
(defconst py-pdbtrack-stack-entry-regexp
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")


(provide 'init-python)
;;; init-python.el ends here
