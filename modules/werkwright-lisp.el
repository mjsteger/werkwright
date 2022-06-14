(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))

(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(provide 'werkwright-lisp)
