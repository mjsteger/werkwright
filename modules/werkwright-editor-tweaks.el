(setq popper-groups '())


(defun popper-group-by-steggy ()
  "This function should return a string or symbol that is the
name of the group this buffer belongs to. It is called with each
popup buffer as current, so you can use buffer-local variables."
  (cond
   ((s-match "magit: .*" (buffer-name (current-buffer)))
    "magit")
   ((s-match "\\*popper-group\\*:\\(.*\\):.*" (buffer-name (current-buffer)))
    (cadr (s-match "\\*popper-group\\*:\\(.*\\):.*" (buffer-name (current-buffer)))))
   ((buffer-local-boundp (intern "popper-group-variable") (current-buffer))
    popper-group-variable)
    (t (popper-group-by-projectile))
  ))

(defun set-popper-group (&optional arg)
  (interactive "P")
  (let ((pgroup (ivy-completing-read "What popper group: " popper-groups nil nil)))        
    (add-to-list 'popper-groups pgroup)
    (setq-local popper-group-variable pgroup)))
  
(use-package popper
  :init
  (popper-mode +1)
  (popper-echo-mode +1)
  (setq popper-group-function #'popper-group-by-steggy)
  (setq popper-display-control t)
  )

(provide 'werkwright-editor-tweaks)
