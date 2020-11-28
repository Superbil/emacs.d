;;; init-slime.el --- Slime support for Common Lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-expand-slime
  :after slime)


;;; Lisp buffers

(use-package slime
  :init
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  :preface
  (defun sanityinc/slime-setup ()
    "Mode setup function for slime lisp buffers."
    (set-up-slime-hippie-expand))
  :config
  ;; package.el compiles the contrib subdir, but the compilation order
  ;; causes problems, so we remove the .elc files there. See
  ;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
  (mapc #'delete-file
        (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

  (use-package slime-company
    :after slime-company
    :config
    (slime-setup (append '(slime-repl slime-fuzzy) extras)))
  (add-hook 'slime-mode-hook 'sanityinc/slime-setup))


;;; REPL

(use-package slime-repl
  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  :bind (:map slime-repl-mode-map
              ("TAB" . indent-for-tab-command))
  :after paredit
  :ensure nil
  :preface
  (defun sanityinc/slime-repl-setup ()
    "Mode setup function for slime REPL."
    (sanityinc/lisp-setup)
    (set-up-slime-hippie-expand)
    (setq show-trailing-whitespace nil))
  :config
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)
  (add-hook 'slime-repl-mode-hook 'sanityinc/slime-repl-setup))


(provide 'init-slime)
;;; init-slime.el ends here
