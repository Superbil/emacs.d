(use-package org
  :pin org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-M-<up>" . org-up-element)))

;; Various preferences
(setq org-catch-invisible-edits 'show
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-archive-mark-done nil
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-export-kill-product-buffer-when-displayed t
      org-fast-tag-selection-single-key 'expert
      org-hide-leading-stars t
      org-html-validation-link nil
      org-log-done t
      org-support-shift-select t
      org-tags-column 80)


;;; org habit
(setq org-agenda-repeating-timestamp-show-all nil
      org-habit-show-habits-only-for-today nil)


;; Lots of stuff from http://doc.norang.ca/org-mode.html
(use-package ob-ditaa
  :ensure nil
  :after ob
  :preface
  (defun sanityinc/grab-ditaa (url jar-name)
    "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
    ;; TODO: handle errors
    (message "Grabbing " jar-name " for org.")
    (let ((zip-temp (make-temp-name "emacs-ditaa")))
      (unwind-protect
          (progn
            (when (executable-find "unzip")
              (url-copy-file url zip-temp)
              (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                     " " (shell-quote-argument jar-name) " > "
                                     (shell-quote-argument org-ditaa-jar-path)))))
        (when (file-exists-p zip-temp)
          (delete-file zip-temp)))))

  :config
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))


;;; Capturing

(setq org-capture-templates
      (quote (("j" "Interruption" entry
               (file+headline org-default-notes-file "Interruption")
               "* %i%?\n  Capture at %<%Y-%m-%d %H:%M>")
              ("m" "Meeting with somebody" entry
               (file+headline org-default-notes-file "Meeting")
               "* MEETING %i%?\n  Capture at %<%Y-%m-%d %H:%M>")
              ("i" "Idea has to catch up" entry
               (file+headline org-default-notes-file "Idea")
               "* %i%?\n  Capture at %<%Y-%m-%d %H:%M>")
              ("t" "Todo sometings" entry
               (file+headline org-default-notes-file "Tasks")
               "* TODO %i%?\n  Capture at %<%Y-%m-%d %H:%M>")
              ("n" "Note about anything" entry
               (file+headline org-default-notes-file "Note")
               "* %i%?\n  Capture at %<%Y-%m-%d %H:%M>")
              ("r" "Something to reading or learning" entry
               (file+headline org-default-notes-file "Idea")
               "* 讀 %i%? :@Reading:\n  Capture at %<%Y-%m-%d %H:%M>")
              ("w" "Watching something" entry
               (file+headline org-default-notes-file "Idea")
               "* 看 %i%? :@Watching:\n  Capture at %<%Y-%m-%d %H:%M>")
              ("h" "Something will always happend" entry
               (file+headline org-default-notes-file "Habit")
               "* TODO %i%?\n  SCHEDULED: <%<%Y-%m-%d> .+1w>\n  Capture at %<%Y-%m-%d %H:%M>\n  :PROPERTIES:\n  :STYLE: habit\n  :END:"))))


;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(use-package org-agenda
  :ensure nil
  :preface
  ;; Exclude DONE state tasks from refile targets
  (defun sanityinc/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

  (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "A version of `org-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))

  (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
    "A version of `org-agenda-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-agenda-refile goto rfloc no-update)))

  :config
  (setq org-agenda-text-search-extra-files `(agenda-archives ,org-default-notes-file))
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(defadvice org-refile (after sanityinc/save-all-after-refile activate)
  "Save all org buffers after each refile operation."
  (org-save-all-org-buffers))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)

;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "Cyan" :weight bold)
              ("DONE" :foreground "green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "brown" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PROJECT" :inherit font-lock-string-face))))



;;; Tag settings

(setq org-tag-alist '((:startgroup)
                      ("@Home" . ?h)
                      ("@Computer" . ?c)
                      ("@Errands" . ?e)
                      (:endgroup)

                      ("@Reading" . ?r)
                      (:startgrouptag)
                      ("@Reading_book" . ?t)
                      ("@Reading_web" . ?y)
                      (:endgrouptag)

                      ("@Gaming" . ?g)
                      ("@Watching" . ?w)))



;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

(defun zin/org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))

(let ((active-project-match "/PROJECT"))
  (setq org-stuck-projects `(,active-project-match ("NEXT"))
        org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy '((agenda habit-down time-up user-defined-up effort-up category-keep)
                                      (todo category-up effort-up)
                                      (tags category-up effort-up)
                                      (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("k" "Use org-capture capture somethings"
           org-capture "")
          ("d" "Daily action list"
           agenda ""
           ((org-agenda-ndays 1)
            (tags-todo "NEXT")
            (org-agenda-sorting-strategy
             (quote (time-up priority-down tag-up)))
            (org-deadline-warning-days 0)))
          (" " "GTD"
           ((tags-todo "-CANCELLED-WAITING-HOLD/!NEXT"
                       ((org-agenda-overriding-header "What you should doing right now!")
                        (org-agenda-skip-function t)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)
                        (org-agenda-todo-ignore-with-date t)))
            (agenda "" ((org-agenda-ndays 7)
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'todo '("WAITING" "HOLD" "CANCELLED" "DONE")))))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)))
            (tags-todo "MEETING"
                       ((org-agenda-overriding-header "Meeting")
                        (org-tags-match-list-sublevels nil)))
            (tags-todo "@Errands/-DONE-NEXT-HOLD"
                       ((org-agenda-overriding-header "List TODO with @Errands")))
            (tags-todo "@Home/-DONE-NEXT-HOLD"
                       ((org-agenda-overriding-header "List TODO with @Home")))
            (tags-todo "@Computer/-DONE-NEXT-HOLD"
                       ((org-agenda-overriding-header "List TODO with @Computer")))
            (tags-todo "{@Reading.*}/-DONE-NEXT-HOLD"
                       ((org-agenda-overriding-header "List TODO with @Reading")))
            (tags-todo "@Watching/-DONE-NEXT-HOLD"
                       ((org-agenda-overriding-header "List TODO with @Watching")))
            (tags-todo "/HOLD"
                       ((org-agenda-overriding-header "On Hold")
                        ;; TODO: skip if a parent is WAITING or HOLD
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        ;; TODO: skip if a parent is a project
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO"))
                                (zin/org-agenda-skip-tag "@Errands")
                                (zin/org-agenda-skip-tag "@Home")
                                (zin/org-agenda-skip-tag "@Computer")
                                (zin/org-agenda-skip-tag "@Reading")
                                (zin/org-agenda-skip-tag "@Watching")
                                (zin/org-agenda-skip-tag "@Gaming"))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ))
          ("h" . "GTD contexts")
          ("he" "Errands" tags-todo "@Errands")
          ("hh" "Home" tags-todo "@Home")
          ("hc" "Computer" tags-todo "@Computer")
          ("hr" "Reading" tags "{@Reading.*}")
          ("hw" "Watching" tags "@Watching")
          ("hg" "Gaming" tags-todo "@Gaming")
          ;; TODO: read tags from org-tag-alist
          ("r" "Tasks to Refile" tags "@Reading|@Reading_web|@Reading_book|@Home|@Computer|@Watching|@Gaming"
           ((org-agenda-files `(,org-default-notes-file))
            (org-agenda-overriding-header "List all from `org-default-notes-file'")))
          )))


(use-package hl-line
  :after org-agenda
  :ensure nil
  :config
  (add-hook 'org-agenda-mode-hook 'hl-line-mode))

;; Agenda speedup and optimization http://orgmode.org/worg/agenda-optimization.html
(setq org-agenda-use-tag-inheritance '(search timeline agenda))


;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(use-package org-clock
  :ensure nil
  :init
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)

  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; while minutes lass than 2, don't log
  (setq org-clock-rounding-minutes 2)

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  ;; Let clock can show all range
  (setq org-clock-display-default-range 'untilnow)

  :preface
  ;; Remove empty LOGBOOK drawers on clock out
  (defun sanityinc/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point))))

  :config
  (org-clock-persistence-insinuate)

  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/remove-empty-drawer-on-clock-out 'append)

  ;;; Install org-clock-statusbar.app from https://github.com/koddo/org-clock-statusbar-app
  (when (and *is-a-mac* (file-directory-p "~/Applications/org-clock-statusbar.app"))
    (add-hook 'org-clock-in-hook
              (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                  (concat "tell application \"org-clock-statusbar\" to clock in \"" (replace-regexp-in-string "\"" "\\\\\"" org-clock-current-task) "\""))))
    (add-hook 'org-clock-out-hook
              (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                  "tell application \"org-clock-statusbar\" to clock out"))))

  :bind
  (:map org-clock-mode-line-map
        ([header-line mouse-2] . org-clock-goto)
        ([header-line mouse-1] . org-clock-menu)))


;;; Archiving
(setq org-archive-mark-done nil)


;;; Calfw org
(use-package calfw
  :after calendars
  :config
  (require 'calfw-org))


;;; Org-mac-iCal
(use-package org-mac-iCal
  :if *is-a-mac*)
;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(use-package org-mac-link
  :if *is-a-mac*
  :ensure nil
  :bind (:map org-mode-map
              ("M-h" . nil)
              ("C-c g" . org-mac-grab-link))
  :init
  (setq org-mac-grab-Evernote-app-p nil
        org-mac-grab-Skim-app-p nil
        org-mac-grab-Acrobat-app-p nil
        org-mac-grab-Outlook-app-p nil
        org-mac-grab-devonthink-app-p nil))

(use-package org-bullets)

(use-package org-fstree
  :after org)

(use-package org-cliplink
  :after org)

(use-package gnuplot
  :after org-babel
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t))))


(provide 'init-org)
