(use-package anything
  :preface
  (defvar anything-c-source-objc-headline
    '((name . "Objective-C Headline")
      (headline  "^[-+@]\\|^#pragma mark")))

  (defun objc-headline ()
    (interactive)
    ;; Set to 500 so it is displayed even if all methods are not narrowed down.
    (let ((anything-candidate-number-limit 500))
      (anything-other-buffer '(anything-c-source-objc-headline)
                             "*ObjC Headline*"))))

(use-package objc-mode
  :mode ("\\.m$" "\\.mm$")
  :ensure nil
  :preface
  (defun objc-keys ()
    (local-set-key (kbd "C-x p") 'objc-headline))
  :config
  (add-hook 'objc-mode-hook 'objc-keys))


(provide 'init-objc)
