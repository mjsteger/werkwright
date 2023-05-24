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
                           ;; (company-mode)
                           (when (< (count-lines (point-min) (point-max)) 20000)

                             (progn
                               (lsp)
                               ;; If you deferred you'd have to chain on setting the flycheck checker. Blegh
                               ;; (lsp-deferred)
                               ;; (flycheck-add-next-checker 'go-errcheck '(warning . lsp))
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


;; I'm sure there's a better way, but boy is it a pain to debug
(flycheck-remove-next-checker 'go-gofmt 'go-golint)
(flycheck-remove-next-checker 'go-gofmt 'go-vet)
(flycheck-remove-next-checker 'go-gofmt 'go-build)
(flycheck-remove-next-checker 'go-gofmt 'go-test)
(flycheck-remove-next-checker 'go-gofmt 'go-errcheck)
(flycheck-remove-next-checker 'go-gofmt 'go-unconvert)
(flycheck-remove-next-checker 'go-gofmt 'go-staticcheck)

(flycheck-remove-next-checker 'go-errcheck 'go-staticcheck)

(flycheck-remove-next-checker 'go-golint 'go-errcheck)
(flycheck-remove-next-checker 'go-golint 'go-unconvert)
(flycheck-remove-next-checker 'go-golint 'go-build)
(flycheck-remove-next-checker 'go-golint 'go-test)

(flycheck-remove-next-checker 'go-vet 'go-build)
(flycheck-remove-next-checker 'go-vet 'go-test)
(flycheck-remove-next-checker 'go-vet 'go-errcheck)
(flycheck-remove-next-checker 'go-vet 'go-unconvert)
(flycheck-remove-next-checker 'go-vet 'go-staticcheck)



(flycheck-add-next-checker 'go-gofmt 'go-staticcheck)
(flycheck-add-next-checker 'go-staticcheck 'go-errcheck)
(flycheck-add-next-checker 'go-errcheck 'go-unconvert)
(flycheck-add-next-checker 'go-unconvert 'go-golint)
(flycheck-add-next-checker 'go-golint 'go-vet)



(provide 'werkwright-go)
