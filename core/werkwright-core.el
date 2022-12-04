(defun werkwright-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory werkwright-dir 0))

(defun werkwright-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(provide 'werkwright-core)
