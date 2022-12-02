(use-package lsp-mode
  :config
  (setq lsp-ui-doc-enable nil) 
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  

  ;; optionally if you want to use debugger
  (setq lsp-eldoc-render-all nil)
  )

;; Per the dude who wrote lsp - https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property))) 

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(use-package yasnippet
  :config
  (require 'yasnippet)
  (yas-global-mode)
  (add-hook 'snippet-mode-hook #'(lambda ()(setq-local require-final-newline nil)))
  (global-set-key (kbd "M-i") 'company-yasnippet)
  )
(use-package yasnippet-snippets
  :config
  (require 'yasnippet-snippets))


(provide 'werkwright-lsp)
