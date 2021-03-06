;;; init-ibuffer.el --- Custom ibuffer  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/


(use-package ibuffer-vc
  :after ibuffer
  :preface
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  :hook
  (ibuffer . ibuffer-set-up-preferred-filters)

  :config
  (setq-default ibuffer-show-empty-filter-groups nil))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :ensure nil
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
  ;; Modify the default ibuffer-formats (toggle with `)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 70 70 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark modified read-only vc-status-mini " "
                (name 70 70 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (fullframe ibuffer ibuffer-quit))


(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
