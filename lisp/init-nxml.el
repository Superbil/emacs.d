;;; init-nxml.el --- Support for editing XML with NXML -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nxml-mode
  :ensure nil
  :mode ("<\\?xml " . nxml-mode)
  :config
  (add-auto-mode 'nxml-mode
                 (concat "\\."
                         (regexp-opt
                          '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
                            "gpx" "tcx" "plist"))
                         "\\'"))
  (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
  (fset 'xml-mode 'nxml-mode)
  (setq nxml-slash-auto-complete-flag t)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (set (make-local-variable 'ido-use-filename-at-point) nil))))


;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun sanityinc/pp-xml-region (beg end)
  "Pretty format XML markup in region from BEG to END.
The function insert linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's indentation rules."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  ;; Use markers because our changes will move END
  (setq beg (set-marker (make-marker) begin)
        end (set-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "\>[ \\t]*\<" end t)
      (backward-char) (insert "\n"))
    (nxml-mode)
    (indent-region begin end)))


(provide 'init-nxml)
;;; init-nxml.el ends here
