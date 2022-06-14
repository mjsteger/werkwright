(global-subword-mode 1)
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (equal arg nil)
        (subword-backward-kill 1)
      (subword-backward-kill arg))))

(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)

(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "C-@") 'er/expand-region)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "M-g") 'goto-line-with-feedback)
(defun authorize-comment ()
  (interactive)
  (insert (concat "steggy: " (format-time-string "%Y-%m-%d:"))))

(global-set-key (kbd "C-x .") 'authorize-comment)

(defun github-for-source (use-current-branch)
  (interactive "P")
  (when-let
      (containing-directory (expand-file-name (locate-dominating-file (buffer-file-name) ".git")))
    (let* ((project-name (first (last (s-split "/" containing-directory t))))
           (path (s-chop-prefix containing-directory (buffer-file-name)))
           (current-line (cadr (s-split " " (what-line))))
           (branch (if use-current-branch (magit-get-current-branch) "master"))
           )
      (kill-new (concat
        "https://github.internal.digitalocean.com/digitalocean/"
        project-name
        "/blob/" branch "/"
        path
        "#L" current-line))))
  )

(defun goto-current-tickler-day ()
  (interactive)
  (cl-multiple-value-bind (month day) (s-split " " (format-time-string "%B %d"))
    (find-file (concat home common-notes-prefix "tickler/" month "/" (first (last (s-split "^0" day))) ".org"))
    )
  )

(setq journal-template "* ")

(defun goto-current-journal-day ()
  (interactive)
  (cl-multiple-value-bind (month day year) (s-split " " (format-time-string "%m %d %Y"))
    (find-file (concat home common-notes-prefix "/gtd/" "journal/" year "-" month "-" (first (last (s-split "^0" day))) ".org"))
    (if (eq (buffer-size (current-buffer)) 0)
        (insert journal-template))
    )
  )

(global-set-key (kbd "C-c e") 'goto-current-tickler-day)
(global-set-key (kbd "C-c z") 'goto-current-journal-day)

(defun ms/make-right-tiny (&optional arg)
  (interactive "P")
  (let ((increase (or current-prefix-arg 100)))
    (enlarge-window-horizontally increase)
  ))

(defun ms/make-left-tiny (&optional arg)
  (interactive "P")
  (let ((increase (or current-prefix-arg 100)))
    (enlarge-window-horizontally (* -1 increase))
    ))

(defun ms/pearing ()
  (interactive "P")
  (set-face-attribute 'default nil :height 120))

(provide 'werkwright-efuncs)
