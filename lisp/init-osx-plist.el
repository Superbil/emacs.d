;; Allow editing of binary .plist files.
(when *is-a-mac*
  (add-to-list 'jka-compr-compression-info-list
               ["\\.nib$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])

  ;;It is necessary to perform an update!
  (jka-compr-update)

  (defun visit-bplist ()
    (interactive)
    (when (string-match "^bplist" (buffer-string))
      (when buffer-read-only (setq buffer-read-only nil))
      (shell-command-on-region
       (point-min) (point-max)
       (format "plutil -convert xml1 -o - %s"
               (shell-quote-argument (buffer-file-name))) t t)
      (save-excursion
        (goto-char (point-max)) (previous-line 1))
      (xml-mode)
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil))))


(provide 'init-osx-plist)
