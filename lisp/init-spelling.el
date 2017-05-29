(require 'ispell
         :ensure nil)

(when (executable-find ispell-program-name)
  (require 'init-flyspell))


(provide 'init-spelling)
