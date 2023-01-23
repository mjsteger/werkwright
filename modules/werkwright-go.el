(use-package dap-mode)

(require 'werkwright-lsp)
(require 'lsp-go)

(require 'dap-dlv-go)
(require 'dap-hydra)
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(use-package company
  :config
  (setq company-backends (-concat '((company-capf)) company-backends)))

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode)
              (setq my-flycheck-local-cache '((go-gofmt . ((next-checkers . (go-golint)))))))))

(add-hook 'go-mode-hook '(lambda ()
                           (company-mode)
                           (when (< (count-lines (point-min) (point-max)) 20000)

                             (progn
                               (lsp)
                               ;; If you deferred you'd have to chain on setting the flycheck checker. Blegh
                               ;; (lsp-deferred)
                               (setq-local flycheck-checker 'go-gofmt)
                               (flycheck-select-checker 'go-gofmt)
                               (setq-local flycheck-checker 'go-gofmt)
                               ))

                           (keymap-local-set "M-." #'lsp-find-definition)
                           (add-hook 'before-save-hook 'gofmt-before-save)
                           (add-hook 'before-save-hook #'lsp-organize-imports)
                           (setq gofmt-show-errors nil)
                           ;; Because this lags big time on any large codebase
                           (setq lsp-enable-file-watchers nil)))


(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.experimentalPackageCacheKey" t t)
   ;; ("gopls.memoryMode" "Normal" nil)
   ("gopls.staticcheck" t t)))

(setq flycheck-error-list-format `[("Line" 20 flycheck-error-list-entry-< :right-align t)
    ("Col" 10 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 20 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)])

(use-package gotest
  :defines popper-reference-buffers
  :init
  ;; If you don't require it first, if it requires in later it will overwrite our binds here
  (require 'go-mode)
  :bind (:map go-mode-map
              (("C-c C-c C-f" . go-test-current-file)
               ("C-C C-C C-c" . go-test-current-test)
              ("C-c C-c C-t" . go-test-current-project)
              )))

(use-package go-playground)


(provide 'werkwright-go)
