(use-package org
  :hook (org-mode . ms/org-mode-setup)
  :config
  (defun ms/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1))
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

;; Org stuff shamelessly stolen from https://systemcrafters.net/emacs-from-scratch/org-mode-basics/
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; (dolist (face '((org-level-1 . 1.2)
;;                 (org-level-2 . 1.1)
;;                 (org-level-3 . 1.05)
;;                 (org-level-4 . 1.0)
;;                 (org-level-5 . 1.1)
;;                 (org-level-6 . 1.1)
;;                 (org-level-7 . 1.1)
;;                 (org-level-8 . 1.1)))
;;     (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defvar home (expand-file-name "~/"))
(setq org-return-follows-link t)
(defvar common-notes-prefix "Dropbox/national/gtd/")

(defun make-org-name (name)
  (concat home common-notes-prefix name ".org"))

(defun add-org-name-to-agenda (name func)
  (add-to-list 'org-agenda-files (apply func name nil)))

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
         "todo"
         )
  (add-org-name-to-agenda it 'make-org-name))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-capture)

(setq org-capture-templates (quote (("i" "Inbox" entry (file "~/national/gtd/inbox.org")
                                     "** TODO %?\n %i Filed on: %U\n")
                                    ("P" "Projects" entry (file "~/national/gtd/projects.org")
                                     "** %?\n %i Filed on: %U\n")
                                    ("c" "Calls" entry (file "~/national/gtd/inbox.org")
                                     "** TODO %?\n :@calls:  %i Filed on: %U\n" )
                                    ("o" "work" entry (file "~/national/gtd/inbox.org")
                                     "** TODO %? :@work:\n  %i Filed on: %U\n")
                                    ("T" "Consulting" entry (file "~/national/gtd/consulting.org")
                                     "** TODO %?\n  %i Filed on: %U\n")
                                    ("h" "Home" entry (file "~/national/gtd/home.org")
                                     "** TODO %?\n  %i Filed on: %U\n")
                                    ("A" "Anywhere" entry (file "~/national/gtd/anywhere.org")
                                     "** TODO %?\n  %i Filed on: %U\n")
                                    ("j" "Jamie Agenda" entry (file "~/national/gtd/agenda/jamie.org")
                                     "** Scheduled %?\n  %i Filed on: %U\n")
                                    ("a" "Appointments" entry (file "~/national/gtd/appointments.org")
                                     "** Scheduled %?\n  %i Filed on: %U\n")
                                    ("e" "Errands" entry (file "~/national/gtd/errands.org")
                                     "** TODO %?\n  %i Filed on: %U\n")
                                    ("C" "At computer" entry (file "~/national/gtd/at_computer.org")
                                     "** TODO %?\n  %i Filed on: %U\n")
                                    ("s" "Someday/Maybe" entry (file "~/national/gtd/someday-maybe.org")
                                     "** %?\n %i Filed on: %U\n")
                                    ("r" "Read-review" entry (file "~/national/gtd/read-review.org")
                                     "** TODO %?\n %i Filed on: %U\n")
                                    ("S" "Standup" entry (file "~/national/gtd/agenda/standup.org")
                                     "** Scheduled %?\n  %i Filed on: %U\n")
                                    ("E" "Emotion Tracking" entry (file "~/national/gtd/emotion_tracking.org")
                                     "* %U\n** What are you feeling? Where are you feeling it?[[https://humansystems.co/emotionwheels/][See this]]%?\n** What Happened?\n** What was the situation/trigger?\n** What did you feel?\n** How did you react?\n** What were the consequences(good/bad) of that reaction?"))))

(setq org-agenda-skip-scheduled-if-done t)

(setq org-todo-keywords
      '((sequence "TODO(t!/!)" "WAITING(w@/@)" "DEFERRED(l!/!)" "Scheduled(s!/!)" "|" "DONE(d!)" "DELEGATED(g@)" "CANCELED(c@)")))

(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %5Effort{+} %CLOCKSUM")
(setq org-fast-tag-selection-include-todo t)


(use-package org-jira)
(setq org-agenda-skip-additional-timestamps-same-entry t)

(use-package org-pomodoro
  :after org
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t))




(use-package pomidor
  :custom
  (pomidor-sound-tick nil)
  (pomidor-sound-tack nil))

; (setq org-agenda-entry-types '(:deadline :scheduled :timestamp :sexp))
(setq org-agenda-window-setup 'current-window)

(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-refile-targets
      `((nil :maxlevel . 1)
        (,(make-org-name "someday-maybe") :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

;; Allow new nodes/setup so that ivy doesn't get sad
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

(setq org-tag-alist '(
                      ("@work" . ?o)
                      ("@home" . ?h)
                      ("@computer" . ?m)
                      ("@calls" . ?a)
                      ("@anywhere" . ?y)
                      ("@errand" . ?e)))

;; Shamelessly stolen from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(setq org-agenda-custom-commands
      '(("o" "Work related stuff" tags-todo "@work"
         ((org-agenda-overriding-header "Work")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("h" "Home related stuff" tags-todo "@home"
         ((org-agenda-overriding-header "Home")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("c" "Computer related stuff" tags-todo "@computer"
         ((org-agenda-overriding-header "Computer")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-q") #'counsel-org-tag))

(provide 'werkwright-org)
