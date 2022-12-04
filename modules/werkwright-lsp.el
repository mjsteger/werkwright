(use-package lsp-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  ;; optionally if you want to use debugger
  (setq lsp-eldoc-render-all nil))

;; Add buffer local Flycheck checkers after LSP for different major modes.
(defvar-local my-flycheck-local-cache nil)
(defun my-flycheck-local-checker-get (fn checker property)
  ;; Only check the buffer local cache for the LSP checker, otherwise we get
  ;; infinite loops.
  (if (eq checker 'lsp)
      (or (alist-get property my-flycheck-local-cache)
          (funcall fn checker property))
    (funcall fn checker property)))


(advice-add 'flycheck-checker-get
            :around 'my-flycheck-local-checker-get)

(use-package yasnippet
  :config
  (yas-global-mode)
  (add-hook 'snippet-mode-hook #'(lambda ()(setq-local require-final-newline nil)))
  (keymap-global-set "M-i" 'company-yasnippet))

(use-package yasnippet-snippets)

(provide 'werkwright-lsp)
