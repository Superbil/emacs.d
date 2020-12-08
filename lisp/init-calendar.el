;;; init-calendar.el --- Custom about calendar -*- lexical-binding: t -*-
;;; Commentary:

;;; Copy from https://gist.github.com/kanru/1a7c4dc58fda760b4bb4

;;; Code:

(eval-when-compile (require 'cl-lib))

(use-package calendar
  :bind (("C-c c d" . insert-date))
  :init
  (defcustom insert-date-format "%Y-%m-%d"
    "This will use for `insert-date'."
    :type 'string
    :group 'calendar)
  :preface
  (defun calendar-date->time (calendar-date)
    "Convert a CALENDAR-DATE as returned from the calendar to a time."
    (encode-time 0 0 0                   ; second, minute, hour
                 (nth 1 calendar-date)   ; day
                 (nth 0 calendar-date)   ; month
                 (nth 2 calendar-date))) ; year

  (defun date-from-calendar ()
    "Insert date from `calendar', it will use `insert-date-format' to format date-string."
    (lexical-let ((date-string nil))
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map calendar-mode-map)
         (define-key map (kbd "RET")
           (lambda ()
             (interactive)
             (switch-to-buffer calendar-buffer)
             (setf date-string (format-time-string insert-date-format
                                                   (calendar-date->time
                                                    (calendar-cursor-to-date t))))
             (calendar-exit)
             (insert date-string)))
         map)
       (lambda () (null date-string)))
      (save-excursion
        (calendar))))

  (defun insert-date (prefix)
    "Insert the current date.
With PREFIX argument, use `format-time-string' by variable `insert-date-format'.
With two PREFIX arguments, use `date-from-calendar' to chose date."
    (interactive "P")
    (cond
     ((not prefix)
      (insert (format-time-string insert-date-format)))
     ((equal prefix '(4))
      (date-from-calendar))))
  :hook
  (calendar-mode . sanityinc/no-trailing-whitespace))


(provide 'init-calendar)
;;; init-calendar.el ends here
