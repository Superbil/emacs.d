;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure nil
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)))

;;; Setup elpy
(use-package elpy)

;; Emacs IPython Notebook
(use-package ein
  :commands (ein:notebooklist-open))

(use-package pip-requirements)

(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :after (company python-mode)
    :config
    (push 'company-anaconda company-backends))
  ;; Fix some reading error https://github.com/pythonic-emacs/anaconda-mode#faq
  (when *is-a-mac*
    (setq anaconda-mode-localhost-address "localhost")))

(use-package pythonic)

;;----------------------------------------------------------------------------
;; On-the-fly syntax checking via flymake
;;----------------------------------------------------------------------------
(use-package flymake-python-pyflakes
  :after python-mode
  :hook (python-mode . flymake-python-pyflakes-load))


;;----------------------------------------------------------------------------
;; pdbtrack constants
;;----------------------------------------------------------------------------
(defconst py-pdbtrack-stack-entry-regexp
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")


(provide 'init-python)
;;; init-python.el ends here
