(use-package pomidor
  :config
  (setq pomidor-seconds (* 30 60))
  (setq pomidor-break-seconds (* 15 60))
  (when (eq system-type 'gnu/linux)
    (progn
      (use-package sound-wav)
      ;; as on e.g. ubuntu I wasn't able to play without setting this up
      (setq pomidor-play-sound-file #'sound-wav-play)))
  )

;; (add-hook 'kill-emacs-hook '(lambda () (pomidor-save-session "yes")))

(provide 'werkwright-pomidor)
