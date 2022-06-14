(use-package exwm-edit
  :config
  (require 'exwm-edit))

(require 'exwm-edit)


;; For the love of god, just give me emacs keybindings
;; Because otherwise I'm gonna accidentally open new windows every time I want to scroll a bit
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (cond ((and exwm-class-name
                       ;; Firefox
                        (s-matches? ".*Nightly.*" exwm-class-name))
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
                      )
                    )
                   )
                  ((and exwm-class-name
                       ;; Firefox
                        (s-matches? ".*Nightly:Meet.*" exwm-class-name))
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
                      ([x] . [up])
                      ([?\C-d] . [delete])
                      ([?\C-k] . [S-end ?\C-c delete])
                      ([?\C-x ?h] . [?\C-a])
                      )
                    )
                   )
                  ((and exwm-class-name
                       ;; Slack
                        (s-matches? ".*Slack:Slack.*" exwm-class-name))
                   (exwm-input-set-local-simulation-keys                    
                     (list (cons 
                            (vconcat (listify-key-sequence (kbd "C-x h")))
                            (vconcat (listify-key-sequence (kbd "C-a")))
                            )
                           (cons 
                            (vconcat (listify-key-sequence (kbd "C-w")))
                            (vconcat (listify-key-sequence (kbd "<backspace>")))
                            )
                           (cons 
                            (vconcat (listify-key-sequence (kbd "C-y")))
                            (vconcat (listify-key-sequence (kbd "C-v")))
                            )
                           ) 
                     )
                    )
                   ))
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
           (if (<= (length exwm-title) 30) exwm-title
             (concat (substring exwm-title 0 29) "...")))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

;; TODO:
;; Should update to allow C-d to go through on meet if the exwm-title changes to Meet something
;; TODO: should allow workspaces, but I need to either fix ace, or better, hack winum mode understand letters instead of numbers(and don't wait for enter to do the thing, christ)
;; (setq exwm-workspace-number 4)
(setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-o] . ace-window)
            ([?\s-f] . exwm-input-toggle-keyboard)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ([?\s-p] . multi-vterm-prev)
            ([?\s-n] . multi-vterm-next)
            ([?\s-e] . multi-vterm)
            ([?\s-i] . pomidor)
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
            ;; ([?\s-\(] . (lambda (command)                         
            ;;              (start-process-shell-command "reset keys" nil "~/bin/unfuck-modmap.sh")))
            ([?\M-\[] . winner-undo)
            ([?\M-\]] . winner-redo)
            ))



(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "DP-2-8" 3 "eDP-1"))
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
             "xrandr" nil "xrandr --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-1-8 --primary --mode 3440x1440 --pos 0x0 --rotate normal --output DP-1-1 --off")
            ))

;; God bless you, daviwil
(defun daviwil-exwm-input--fake-last-command()
  "Fool some packages into thinking there is a change in the buffer."
  (setq last-command #'exwm-input--noop)
  (condition-case hook-error
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
