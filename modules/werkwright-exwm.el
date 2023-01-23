(use-package exwm-edit
  :config
  (require 'exwm-edit))

(defun other-exwm-workspace()
  (interactive)
  (dotimes (i (length exwm-workspace--list))
    (when (equal (nth i exwm-workspace--list) exwm-workspace--current)
      (return (exwm-workspace-switch (mod (+ 1 i) (length exwm-workspace--list)))))))

(use-package hydra
  :config
  (defun get-buffer-by-name (name)
    (-first (lambda (buf) (s-match name (buffer-name buf))) (buffer-list)))

  (defun switch-to-buffer-named (name)
    (pop-to-buffer (get-buffer-by-name name)))

  (defun unfuck-modmap ()
    (interactive)
    (start-process-shell-command "reset keys" nil "~/bin/unfuck-modmap.sh"))

  (defhydra "exwm-buffers" ()
    "common buffers"
    ("m" (lambda () (interactive) (switch-to-buffer-named "firefox-default:Meet - ")) :exit t)
    ("f" (lambda () (interactive) (switch-to-buffer-named "firefox-default:[^(Meet)]")) :exit t)
    ("s" (lambda () (interactive) (switch-to-buffer-named "Slack:")) :exit t)
    ("c" (lambda () (interactive) (switch-to-buffer-named "*scratch*")) :exit t)
    ("g" (lambda () (interactive) (switch-to-buffer-named "groundcontrol")) :exit t)
    ("d" (lambda () (interactive) (switch-to-buffer-named "doit")) :exit t)
    )

  ;; Put here since it's effectively overriding an option here

  (defhydra "exwm-control" (global-map "C-c")
    "exwm"
    ("w" other-exwm-workspace "go to other workspace" :exit t)
    ("o" ace-window "ace around" :exit t)
    ("C-x C-j" org-clock-goto "goto clock" :exit t)
    ("C-x bye" (lambda () (interactive) (start-process-shell-command "go to other screen" nil "/home/msteger/bin/send_monitors")) :exit t)
    ("i" pomidor "pomidor")
    ("u" vterm "vterming" :exit t)
    ("d" popper-toggle-type "toggle popper type" :exit t)
    ("s" popper-toggle-latest "toggle latest popper" :exit t)
    ("h" popper-cycle "cycle popper" :exit t)
    ("t" popper-kill-latest-popup "kill last popper popup" :exit t)
    ("n" multi-vterm-next "next vterm")
    ("y" multi-vterm-prev "prev vterm")
    ("." #'hydra-spotify/body "spotify controls" :exit t)
    ("e" multi-vterm "make a new multi vterm" :exit t)
    ("9" unfuck-modmap "unfuck modmap" :exit t)
    ("[" winner-undo "winner undo")
    ("]" winner-redo "winner redo")
    ("b" \"exwm-buffers\"/body :exit t)
    ))

;; For the love of god, just give me emacs keybindings
;; Because otherwise I'm gonna accidentally open new windows every time I want to scroll a bit
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (cond
             ((and exwm-class-name
                       ;; Firefox
                        (s-matches? ".*firefox-default:Meet -.*" exwm-class-name))
                   (exwm-input-set-local-simulation-keys
                    '(([?\C-p] . [up])
                      ([?\C-y] . [?\C-v])
                      ([?\C-n] . [down])
                      ([?\C-a] . [home])
                      ([?\C-w] . [C-backspace])
                      ([?\C-s] . [C-f C-g])
                      ([?\C-r] . [C-S-g])
                      ([?\C-f] . [C-tab])
                      ([?\C-b] . [C-S-tab])
                      ([?\C-e] . [end])
                      ([?\C-k] . [C-w])
                      ([?\C-d] . [delete])
                      ([?\C-k] . [S-end ?\C-c delete])
                      ([?\C-x ?h] . [?\C-a])
                      ([?\C-i] . [?\C-d])
                      )))
             ((and exwm-class-name
                       ;; Firefox
                        (s-matches? ".*firefox-default.*" exwm-class-name))
                   (exwm-input-set-local-simulation-keys
                    '(([?\C-p] . [up])
                      ([?\C-y] . [?\C-v])
                      ([?\C-n] . [down])
                      ([?\C-a] . [home])
                      ([?\C-w] . [C-backspace])
                      ([?\C-s] . [C-f C-g])
                      ([?\C-r] . [C-S-g])
                      ([?\C-f] . [C-tab])
                      ([?\C-b] . [C-S-tab])
                      ([?\C-e] . [end])
                      ([?\C-k] . [C-w])
                      ([?\C-d] . [delete])
                      ([?\C-k] . [S-end ?\C-c delete])
                      ([?\C-x ?h] . [?\C-a])
                      ([?\C-i] . [?\C-d])
                      )))
                  ((and exwm-class-name
                       ;; Slack
                        (s-matches? ".*Slack:.*" exwm-class-name))
                   (exwm-input-set-local-simulation-keys
                    (list
                     (cons [?\C-*] [?\s])
                     (cons
                      (vconcat (listify-key-sequence (kbd "C-x h")))
                      (vconcat (listify-key-sequence (kbd "C-a"))))
                     (cons
                      (vconcat (listify-key-sequence (kbd "C-w")))
                      (vconcat (listify-key-sequence (kbd "<backspace>"))))
                     (cons
                      (vconcat (listify-key-sequence (kbd "C-y")))
                      (vconcat (listify-key-sequence (kbd "C-v"))))
                     )
                    ))))
            ;; (when (and exwm-class-name
            ;;            ;; Firefox
 ;;            (s-matches? ".*Nightly.*" exwm-class-name))
            ;;   (exwm-input-set-local-simulation-keys
            ;;    '(([?\C-p] . [up])
            ;;      ([?\C-y] . [?\C-v])
            ;;      ([?\C-n] . [down])
            ;;      ([?\C-a] . [home])
            ;;      ([?\C-w] . [C-backspace])
            ;;      ([?\C-s] . [C-f C-g])
            ;;      ([?\C-r] . [C-S-g])
            ;;      ([?\C-f] . [C-tab])
            ;;      ([?\C-b] . [C-S-tab])
            ;;      ([?\C-e] . [end])
            ;;      ([?\C-d] . [delete])
            ;;      ([?\C-k] . [S-end ?\C-c delete]))
            ;;    ))
            )

(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

;; TODO: should allow workspaces, but I need to either fix ace, or better, hack winum mode understand letters instead of numbers(and don't wait for enter to do the thing, christ)
(setq exwm-workspace-number 2)
(setq exwm-input-global-keys
      `(
        ([?\s-q] . (lambda ()
                     (interactive)
                     (start-process-shell-command "reset monitor" nil "/home/msteger/bin/fix-monitors")))
        ([?\s-*] . (lambda ()
                     (interactive)
                          (start-process-shell-command "fix brightness on top monitor" nil "/home/msteger/bin/fix-brightness")))
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . other-exwm-workspace)
            ([?\C-c ?w] . other-exwm-workspace)
            ([?\s-o] . ace-window)
            ([?\C-c ?o] . ace-window)
            ([?\s-f] . exwm-input-toggle-keyboard)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ([?\s-p] . multi-vterm-prev)
            ([?\s-n] . multi-vterm-next)
            ([?\s-e] . multi-vterm)
            ([?\s-u] . vterm)
            ([?\s-i] . pomidor)
            ([?\C-c ?i] . pomidor)
            ([?\s-a] . popper-toggle-type)
            ([?\s-s] . popper-toggle-latest)
            ([?\s-h] . popper-cycle)
            ([?\s-t] . popper-kill-latest-popup)
            ([?\s-.] . smudge-command-map)
            ;; ;; 's-N': Switch to certain workspace.
            ;; ,@(mapcar (lambda (i)
            ;;             `(,(kbd (format "s-%d" i)) .
            ;;               (lambda ()
            ;;                 (interactive)
            ;;                 (exwm-workspace-switch-create ,i))))
            ;;           (number-sequence 0 9))
            ([?\s-\(] . (lambda ()
                          (interactive)
                          (start-process-shell-command "reset keys" nil "~/bin/unfuck-modmap.sh")))
            ([?\M-\[] . winner-undo)
            ([?\M-\]] . winner-redo)
            ))



(require 'exwm-randr)

(setq exwm-randr-workspace-output-plist '(0 "HDMI-1" 1 "DP-1-8"))

(exwm-randr-enable)

(defun main-screen-turn-on()
  (interactive "P")
  (start-process-shell-command
   "xrandr" nil "xrandr --output eDP-1 --off --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-1-8 --primary --mode 3440x1440 --pos 0x0 --rotate normal --output DP-1-1 --off"))

(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            ;; (start-process-shell-command
            ;;  "xrandr" nil "xrandr --output eDP-1 --off --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-1-8 --primary --mode 3440x1440 --pos 0x0 --rotate normal --output DP-1-1 --off")
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --mode 1920x1080 --pos 3440x1800 --rotate normal --output HDMI-1 --primary --mode 3440x1440 --pos 0x1440 --rotate normal --output DP-1 --off --output DP-2 --off --output DP-1-8 --mode 3440x1440 --pos 0x0 --rotate inverted --output DP-1-1 --off")
             ;; "xrandr" nil "xrandr --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-1-8 --primary --mode 3440x1440 --pos 0x0 --rotate normal --output DP-1-1 --off")
            ))


;; God bless you, daviwil
(defun daviwil-exwm-input--fake-last-command()
  "Fool some packages into thinking there is a change in the buffer."
  (setq last-command #'exwm-input--noop)  (condition-case hook-error
      (progn
        (run-hooks 'pre-command-hook)
        (run-hooks 'post-command-hook))
    ((error)
     (exwm--log "Error occurred while running command hooks: %s\n\nBacktrace:\n\n%s"
                hook-error
                (with-temp-buffer
                  (setq-local standard-output (current-buffer))
                  (backtrace)
                  (buffer-string))))))

(advice-add 'exwm-input--fake-last-command :override #'daviwil-exwm-input--fake-last-command)



(provide 'werkwright-exwm)
