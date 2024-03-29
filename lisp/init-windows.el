;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;;; -*- lexical-binding: t -*-

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(use-package winner
  :ensure nil
  :init (setq winner-dont-bind-my-keys t)
  :bind
  (("C-x 1" . sanityinc/toggle-delete-other-windows)
   ("C-c h" . winner-undo)
   ("C-c n" . winner-redo))
  :preface
  (defun sanityinc/toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))

  :hook (after-init . winner-mode))


;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :bind
  (("C-x o" . switch-window))
  :init
  (setq-default switch-window-shortcut-style 'qwerty)
  (setq-default switch-window-timeout nil))



;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(bind-key "<f7>" 'sanityinc/split-window)



(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(bind-key "C-c <down>" 'sanityinc/toggle-current-window-dedication)



(use-package windmove
  :if (memq window-system '(nt w32))
  :ensure nil
  :config
  (windmove-default-keybindings 'control))


(provide 'init-windows)
;;; init-windows.el ends here
