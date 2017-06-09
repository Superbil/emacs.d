;;; Find and load the correct package.el

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir
       (expand-file-name "site-lisp/package" user-emacs-directory)))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)



;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (when (file-directory-p package-user-dir)
    (message "Default package locations have changed in this config: renaming old package dir %s to %s."
             package-user-dir
             versioned-package-dir)
    (rename-file package-user-dir versioned-package-dir))
  (setq package-user-dir versioned-package-dir))



;;; Standard package repositories

(add-to-list 'package-archives
             `("org" . "http://orgmode.org/elpa/"))

;;; Also use Melpa for most packages
(add-to-list 'package-archives
             `("melpa" . "https://melpa.org/packages/"))


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

(use-package benchmark-init)

(require 'diminish)
(require 'bind-key)


(use-package fullframe
  :config
  (fullframe list-packages quit-window))


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
