;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

(use-package diff-hl
  :after magit
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (after-init . global-diff-hl-mode)))


(provide 'init-vc)
;;; init-vc.el ends here
