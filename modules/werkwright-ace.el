(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?h ?t ?n ?e ?o ?i ?9))
  (keymap-global-set "M-o" 'ace-window)
  (setq aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?f aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?E aw-execute-command-other-window "Execute Command Other Window")
      (?F aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?O delete-other-windows "Delete Other Windows")
      (?T aw-transpose-frame "Transpose Frame")
      ;; ?i ?r ?t are used by hyperbole.el
      (?? aw-show-dispatch-help))))

(provide 'werkwright-ace)
