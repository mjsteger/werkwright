(use-package org-pomodoro)
(use-package org)

(defvar home (expand-file-name "~/"))
(setq org-return-follows-link t)
(defvar common-notes-prefix "Dropbox/national/gtd/")

(defun make-org-name (name)
  (concat home common-notes-prefix name ".org"))

(defun add-org-name-to-agenda (name func)
  (add-to-list 'org-agenda-files (apply func name nil))
  )

(defun make-org-agenda-name (name)
  (concat home common-notes-prefix "agenda/" name ".org"))

(setq org-agenda-files '())

(--each (list
         "calls"
         "office"
         "home"
         "anywhere"
         "errands"
         "at_computer"
         "inbox"
         "projects"
         "read-review"
         "appointments"
         "refile-beorg"
         "consulting"
         "google-cal"
         )
  (add-org-name-to-agenda it 'make-org-name))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)

(setq org-capture-templates (quote (("c" "Calls" entry (file "~/national/gtd/calls.org")
                                     "** TODO %?\n  %i Filed on: %T\n" )
                                    ("o" "Office" entry (file "~/national/gtd/office.org")
                                     "** TODO %?\n  %i Filed on: %T\n")
                                    ("T" "Consulting" entry (file "~/national/gtd/consulting.org")
                                     "** TODO %?\n  %i Filed on: %T\n")
                                    ("h" "Home" entry (file "~/national/gtd/home.org")
                                     "** TODO %?\n  %i Filed on: %T\n")
                                    ("A" "Anywhere" entry (file "~/national/gtd/anywhere.org")
                                     "** TODO %?\n  %i Filed on: %T\n")
                                    ("j" "Jamie Agenda" entry (file "~/national/gtd/agenda/jamie.org")
                                     "** Scheduled %?\n  %i Filed on: %T\n")
                                    ("a" "Appointments" entry (file "~/national/gtd/appointments.org")
                                     "** Scheduled %?\n  %i Filed on: %T\n")
                                    ("J" "Joonas 1-1" entry (file "~/national/gtd/agenda/joonas-1:1.org")
                                     "** Scheduled %?\n  %i Filed on: %T\n")
                                    ("e" "Errands" entry (file "~/national/gtd/errands.org")
                                     "** TODO %?\n  %i Filed on: %T\n")
                                    ("C" "At computer" entry (file "~/national/gtd/at_computer.org")
                                     "** TODO %?\n  %i Filed on: %T\n")
                                    ("i" "Inbox" entry (file "~/national/gtd/inbox.org")
                                     "** %?\n %i Filed on: %T\n")
                                    ("s" "Someday/Maybe" entry (file "~/national/gtd/someday-maybe.org")
                                     "** %?\n %i Filed on: %T\n")
                                    ("S" "Standup" entry (file "~/national/gtd/agenda/standup.org")
                                     "** Scheduled %?\n  %i Filed on: %T\n")
                                    )
                                    ))

(setq org-agenda-skip-scheduled-if-done t)

(setq org-todo-keywords
      '((sequence "TODO(t!/!)" "WAITING(w@/@)" "DEFERRED(l!/!)" "Scheduled(s!/!)" "|" "DONE(d!)" "DELEGATED(d@)" "CANCELED(c@)")))

(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %5Effort{+} %CLOCKSUM")
(setq org-fast-tag-selection-include-todo t)


(provide 'werkwright-org)
