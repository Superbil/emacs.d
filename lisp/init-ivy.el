(use-package ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-j" . ivy-immediate-done)
        ("RET" . ivy-alt-done)
        ("<up>" . ivy-previous-line-or-history))
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-initial-inputs-alist
                '((man . "^")
                  (woman . "^")))
  :config
  (add-hook 'after-init-hook (lambda () (ivy-mode 1)))
  :diminish ivy-mode)

(use-package ivy-historian
  :after ivy-mode
  :config
  (ivy-historian-mode t))

(use-package counsel
  :after ivy-mode
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  :config
  (counsel-mode)
  :diminish counsel-mode)


(provide 'init-ivy)
