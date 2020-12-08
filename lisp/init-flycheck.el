;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :preface
  (defun flycheck-org-lint-start (checker callback)
    (funcall
     callback 'finished
     (save-excursion
       (mapcar
        (lambda (err)
          (goto-char (car err))
          (flycheck-error-new-at
           (org-current-line) (1+ (current-column))
           'warning (cadr err) :checker checker))
        (org-lint-link-to-local-file (org-element-parse-buffer))))))
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

  (use-package org-lint
    :ensure nil
    :config
    (flycheck-define-generic-checker 'org-lint
      "Syntax checker for org-lint."
      :start 'flycheck-org-lint-start
      :modes '(org-mode))
    (add-to-list 'flycheck-checkers 'org-lint)))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
