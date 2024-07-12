(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (equal arg nil)
        (subword-backward-kill 1)
      (subword-backward-kill arg))))

(keymap-global-set "C-w" 'backward-kill-word-or-kill-region)

(keymap-global-set  "C-j" (lambda ()
                  (interactive)
                  (join-line -1)))




(provide 'werkwright-efuncs)
