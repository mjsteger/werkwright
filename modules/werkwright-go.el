(use-package dap-mode)

(require 'werkwright-lsp)
(require 'lsp-go)

(use-package company
  :config
  (setq company-backends (-concat '((company-capf)) company-backends)))

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode)
              (setq my-flycheck-local-cache '((go-gofmt . ((next-checkers . (go-golint)))))))))

(add-hook 'go-mode-hook '(lambda ()
                           (require 'lsp)
                           (company-mode)
                           (when (< (count-lines (point-min) (point-max)) 20000)
                             (progn
                               (lsp-deferred)
                               ))
                           (keymap-local-set "M-." #'lsp-find-definition)
                           (flycheck-select-checker 'go-gofmt)
                           (add-hook 'before-save-hook 'gofmt-before-save)
                           (setq gofmt-show-errors nil)
                           ;; Because this lags big time on any large codebase
                           (setq lsp-enable-file-watchers nil)))

;; unfortunately not available if we want to do the degradeclosed stuff
;; (add-hook 'go-mode-hook #'lsp-organize-imports)
(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.experimentalPackageCacheKey" t t)
   ("gopls.memoryMode" "DegradeClosed" nil)
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
              ))
  :config
  (add-to-list 'popper-reference-buffers 'go-test-mode))

(use-package go-playground)


(provide 'werkwright-go)
