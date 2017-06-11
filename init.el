;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "25"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; More memory on garbage collection
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))
(setq message-log-max 16384)

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Setup scrtch welcome message
;;----------------------------------------------------------------------------
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ❤️ you!\n\n"))

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(use-package command-log-mode)

(require 'init-frame-hooks)
(require 'init-term)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-smex)
(require 'init-ivy)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)

(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-fci)

(require 'init-paredit)
(require 'init-lisp)

(require 'init-vc)
(require 'init-git)
(require 'init-github)

(when *is-a-mac*
  (require 'init-osx))

(require 'init-compile)
(require 'init-textile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
(require 'init-haskell)
(require 'init-ruby-mode)
(require 'init-yaml)
(require 'init-objc)
(require 'init-sql)
(require 'init-eshell)
(require 'init-zsh)

(require 'init-javascript)

(require 'init-osx-plist)

(require 'init-slime)
(require 'init-clojure)
(require 'init-clojure-cider)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)
(require 'init-tramp)

(require 'init-calendar)
(require 'init-folding)
(require 'init-dash)
(require 'init-ledger)
(require 'init-projectile)
(require 'init-twittering)
(require 'init-games)

;; Extra packages which don't require any configuration
(use-package lua-mode)
(use-package osx-location
  :if *is-a-mac*)
(use-package regex-tool
  :commands regex-tool)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

(require 'init-client)
;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


(use-package uptimes
  :config (add-hook 'after-init-hook (lambda () (require 'uptimes))))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
