;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(use-package git-blamed)
(use-package gitignore-mode)
(use-package gitconfig-mode
  :mode ".gitconfig-local\\'")
(use-package git-timemachine)
(use-package git-attr)
(use-package git-link)

(use-package magit
  :bind
  ;; Hint: customize `magit-repo-dirs' so that you can use C-u C-x g to
  ;; quickly open magit on any one of your projects.
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   :map magit-mode-map
   ("M-w" . whole-line-or-region-kill-ring-save))

  :init
  (setq-default magit-diff-refine-hunk nil
                magit-refresh-status-buffer nil
                magit-diff-highlight-trailing nil
                magit-diff-paint-whitespace nil)

  :hook
  ((magit-popup-mode . sanityinc/no-trailing-whitespace)
   (after-save . magit-after-save-refresh-status))

  :config
  (when *is-a-mac*
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

  (use-package magit-section
    :ensure nil
    :bind
    (:map magit-status-mode-map
          ("C-M-<up>" . magit-section-up)))

  (fullframe magit-status magit-mode-quit-window))

(use-package magit-lfs)

;; let magit support issues system, github or gitlab
(use-package forge
  :after magit)

(use-package git-commit
  :hook (git-commit-mode . goto-address-mode))


;; Convenient binding for vc-git-grep
(use-package vc-git
  :bind ("C-x v f" . vc-git-grep)
  :ensure nil)


;;; git-svn support
(use-package magit-svn
  :hook (magit-mode . magit-svn-mode))

(use-package compile
  :ensure nil
  :preface
  (defvar git-svn--available-commands nil "Cached list of git svn subcommands")

  (defun git-svn--available-commands ()
    (or git-svn--available-commands
        (setq git-svn--available-commands
              (sanityinc/string-all-matches
               "^  \\([a-z\\-]+\\) +"
               (shell-command-to-string "git svn help") 1))))

  (defun git-svn (dir command)
    "Run a git svn subcommand in DIR."
    (interactive (list (read-directory-name "Directory: ")
                       (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
    (let* ((default-directory (vc-git-root dir))
           (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
      (compile (concat "git svn " command))))

  :config
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))


;;; git-flow
(use-package magit-gitflow
  ;; redefine key C-f to C-c C-f
  :bind (:map magit-gitflow-mode-map
              ("C-f" . nil)
              ("C-c C-f" . magit-gitflow-popup))
  :hook (magit-mode . turn-on-magit-gitflow))


(provide 'init-git)
;;; init-git.el ends here
