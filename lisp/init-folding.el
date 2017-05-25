(use-package origami
  :after prog-mode
  :bind (:map origami-mode-map
              ("C-c f" . origami-recursively-toggle-node)
              ("C-c F" . origami-toggle-all-nodes))
  :config
  (add-hook 'prog-mode-hook 'origami-mode))


(provide 'init-folding)
