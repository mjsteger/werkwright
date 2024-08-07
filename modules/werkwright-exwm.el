;; -*- lexical-binding: t -*-

(use-package exwm-edit
  :config
  (require 'exwm-edit))

(defun other-exwm-workspace()
  (interactive)
  (dotimes (i (length exwm-workspace--list))
    (when (equal (nth i exwm-workspace--list) exwm-workspace--current)
      (return (exwm-workspace-switch (mod (+ 1 i) (length exwm-workspace--list)))))))

(setq avy-keys '(?a ?s ?h ?t ?n ?e ?o ?i))

(use-package hydra
  :config
  (defun get-buffer-by-name (name)
    (-first (lambda (buf) (s-match name (buffer-name buf))) (buffer-list)))

  ;; If the buffer is displayed, switched to it
  ;; If it's not displayed, then simply make this current window be that buffer
  (defun switch-to-buffer-named (name)
    (let ((buffer (get-buffer-by-name name)))
      (if (get-buffer-window buffer t)
          ;; If it's the same workspace, pop buffer is what we want. if it's not, unfortunately, it's exwm-workspace-switch-to-buffer
          (if (get-buffer-window buffer)
            (pop-to-buffer buffer)
          (exwm-workspace-switch-to-buffer buffer))
        (let ((exwm-layout-show-all-buffers t)
              (exwm-workspace-show-all-buffers t))
          (exwm-workspace-switch-to-buffer buffer)))
      ))

  (defun unfuck-modmap ()
    (interactive)
    (start-process-shell-command "reset keys" nil "~/bin/unfuck-modmap.sh"))





  (defvar exwm-buffers '(("m" (lambda () (interactive) (switch-to-buffer-named "firefox-default:Meet - ")) :exit t)
                         ("f" (lambda () (interactive) (switch-to-buffer-named "firefox-default:[^(Meet)]")) :exit t)
                         ("s" (lambda () (interactive) (switch-to-buffer-named "Slack:")) :exit t)
                         ("c" (lambda () (interactive) (switch-to-buffer-named "*scratch*")) :exit t)
                         ("g" (lambda () (interactive) (switch-to-buffer-named "groundcontrol")) :exit t)
                         ("d" (lambda () (interactive) (switch-to-buffer-named "doit")) :exit t)
                         ))

  (defun add-to-buffer-hotkeys (str)
    (interactive "sWhat letter to assign for this buffer?: ")
    (let ((used-letters (-map 'first exwm-buffers)))
      (if (-contains? used-letters str)
          (message "Cannot use letter %s, please use a different letter. Used letters: %s" str used-letters)
        (setq exwm-buffers (append (list (list str (
                                                    (lambda (buffer-name) (lambda () (interactive) (switch-to-buffer-named buffer-name)))
                                                    (buffer-name))

                                               :exit t))
                                   exwm-buffers))))
    ;; Have to force re-eval it, not using defhydra+ because of multi-level macro quoting in that case
    (re-eval-exwm-buffers)
    )

  (defun remove-from-buffer-hotkeys (str)
    (interactive "sWhat letter to remove?: ")
    (let ((used-letters (-map 'first exwm-buffers)))
      (if (not (-contains? used-letters str))
          (message "Cannot use letter %s to remove as it does not appear, please use a different letter. Used letters: %s" str used-letters)
        (setq exwm-buffers (-filter (lambda (x) (not (equal (car x) str))) exwm-buffers))))

    ;; Have to force re-eval it, not using defhydra+ because of multi-level macro quoting in that case
    (re-eval-exwm-buffers)
    )

  (defun re-eval-exwm-buffers ()
    (eval `(defhydra "exwm-buffers" () "common-buffers" ,@exwm-buffers)))

  (re-eval-exwm-buffers)

  ;; Put here since it's effectively overriding an option here
  (defhydra "exwm-control" (global-map "C-c")
    "exwm"
    ("w" other-exwm-workspace "go to other workspace" :exit t)

    ("o" ace-window "ace around" :exit t)
    ("C-x C-j" org-clock-goto "goto clock" :exit t)
    ("C-x bye" (lambda () (interactive) (start-process-shell-command "go to other screen" nil "/home/msteger/bin/send_monitors")) :exit t)
    ("i" pomidor "pomidor")
    ("u" vterm "vterming" :exit t)
    ;; ("d" popper-toggle-type "toggle popper type" :exit t)
    ;; ("s" popper-toggle-latest "toggle latest popper" :exit t)
    ;; ("h" popper-cycle "cycle popper" :exit t)
    ;; ("t" popper-kill-latest-popup "kill last popper popup" :exit t)
    ("s" add-to-buffer-hotkeys "add-to-buffer-hotkeys" :exit t)
    ("h" remove-from-buffer-hotkeys "remove-from-buffer-hotkeys" :exit t)
    ("f" avy-goto-char "avy gotoword" :exit t)
    ("j" avy-goto-char-2 "avy gotoword" :exit t)
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
                        (s-matches? ".*firefox:Meet -.*" exwm-class-name))
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
                        (s-matches? ".*firefox.*" exwm-class-name))
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

;; (setq exwm-randr-workspace-output-plist '(0 "DP-2" 1 "HDMI-0" 2 "DP-0.8"));
(setq exwm-randr-workspace-output-plist '(0 "DP-0" 1 "HDMI-0"))

;; ;; Uncomment this after back from Seattle
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
