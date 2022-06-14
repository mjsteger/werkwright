(use-package dap-mode
  )

(require 'dap-go)
;; (dap-go-setup)


(require 'werkwright-lsp)
(require 'lsp-go)

(use-package company
  :config
  (push 'company-capf company-backends)
  )

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

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode)
              (setq my-flycheck-local-cache '((go-gofmt . ((next-checkers . (go-golint)))))))))

(add-hook 'go-mode-hook '(lambda ()
                           (require 'lsp)
                           (company-mode)
                           (when (< (count-lines (point-min) (point-max)) 20000)
                             (progn
                               (lsp)
                               (lsp-mode)))
                           (local-set-key (kbd "M-.") #'lsp-find-definition)
                           (rainbow-delimiters-mode)
                           (flycheck-select-checker 'go-gofmt)
                           (add-hook 'before-save-hook 'gofmt-before-save)                           
                           (setq gofmt-show-errors nil)
                           ;; Because this lags big time on any large codebase
                           (setq lsp-enable-file-watchers nil)))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))


(setq flycheck-error-list-format `[("Line" 20 flycheck-error-list-entry-< :right-align t)
    ("Col" 10 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 20 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)])

(provide 'werkwright-go)

