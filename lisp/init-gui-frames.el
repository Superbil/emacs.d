;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;----------------------------------------------------------------------------
;; More memory on garbage collection when use GUI
;;----------------------------------------------------------------------------
(setq gc-cons-threshold most-positive-fixnum)

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(use-package frame
  :ensure nil
  :preface
  (defun sanityinc/maybe-suspend-frame ()
    "Minimizing windows under OS X."
    (interactive)
    (unless (and *is-a-mac* window-system)
      (suspend-frame)))
  :bind ("C-z" . sanityinc/maybe-suspend-frame))


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


(use-package disable-mouse)


;;; fix "empty or unsupported pasteboard type"
;;; http://lists.gnu.org/archive/html/emacs-bug-tracker/2012-09/msg00086.html
(when *is-a-mac*
  (defun ns-get-pasteboard ()
    "Returns the value of the pasteboard, or nil for unsupported formats."
    (condition-case nil
        (ns-get-selection-internal 'CLIPBOARD)
      (quit nil))))


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
