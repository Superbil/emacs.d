;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ispell
         :ensure nil)

(when (executable-find ispell-program-name)
  (require 'init-flyspell))


(provide 'init-spelling)
;;; init-spelling.el ends here
