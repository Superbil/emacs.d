
(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  :init (setq csv-separators '("," ";" "|" " ")))


(provide 'init-csv)
