(use-package sane-term
  :commands sane-term
  :bind (("<f10>" . sane-term)))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


(provide 'init-term)
