(use-package ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-j" . ivy-immediate-done)
        ("RET" . ivy-alt-done))
  :ensure t
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-initial-inputs-alist
                '((man . "^")
                  (woman . "^")))
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (when (bound-and-true-p ido-ubiquitous-mode)
                (ido-ubiquitous-mode -1))
              (when (bound-and-true-p ido-mode)
                (ido-mode -1))
              (ivy-mode 1)))
  :diminish ivy-mode)

(use-package ivy-historian
  :after ivy-mode
  :config
  (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))

(use-package counsel
  :after ivy-mode
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  :config
  (add-hook 'after-init-hook 'counsel-mode)
  :diminish counsel-mode)


(provide 'init-ivy)
