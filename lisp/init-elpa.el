;;; Find and load the correct package.el

(require 'package)


;;; Standard package repositories

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)


(use-package try)

(use-package benchmark-init)

(require 'diminish)
(require 'bind-key)


(use-package fullframe
  :config
  (fullframe list-packages quit-window))


(use-package benchmark-init)


(use-package cl-lib
  :functions (sanityinc/set-tabulated-list-column-width
              sanityinc/maybe-widen-package-menu-columns)
  :preface
  (defun sanityinc/set-tabulated-list-column-width (col-name width)
    "Set any column with name COL-NAME to the given WIDTH."
    (when (> width (length col-name))
      (cl-loop for column across tabulated-list-format
               when (string= col-name (car column))
               do (setf (elt column 1) width))))

  (defun sanityinc/maybe-widen-package-menu-columns ()
    "Widen some columns of the package menu table to avoid truncation."
    (when (boundp 'tabulated-list-format)
      (sanityinc/set-tabulated-list-column-width "Version" 13)
      (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
        (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))
  :config
  (add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns))


(provide 'init-elpa)
