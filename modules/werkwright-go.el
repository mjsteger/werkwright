(use-package dap-mode
  :config
  )

(require 'dap-go)
(dap-go-setup)


(require 'lsp-go)

(use-package company
  :config
  (push 'company-capf company-backends)
  )

(add-hook 'go-mode-hook '(lambda ()
                           (company-mode)                         
                           (lsp)
                           (lsp-mode)
                           (local-set-key (kbd "M-.") #'lsp-find-definition)
                           ))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(setq flycheck-go-golint-executable "/Users/steggy/gocode//bin/golint")


(provide 'werkwright-go)

