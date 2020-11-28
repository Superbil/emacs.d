;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-v" . markdown-preview-with-app))
  :preface
  (defcustom markdown-preview-app "/Applications/Marked.app"
    "Markdown preview app location")

  (defun markdown-preview-with-app ()
    "Run App on the current file if `markdown-preview-app' is installed;
otherwise fallback to `markdown-preview'"
    (interactive)
    (if (file-exists-p markdown-preview-app)
        (superbil/osx-open-app markdown-preview-app (buffer-file-name))
      (markdown-preview)))
  :after whitespace-cleanup-mode
  :config
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))


(provide 'init-markdown)
;;; init-markdown.el ends here
