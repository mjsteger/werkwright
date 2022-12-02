(use-package dap-mode
  )

(require 'dap-go)

;; (dap-go-setup)

(use-package gotest
  :config
  (require 'gotest)
  (require 'gotest-autoloads)
  (add-to-list 'popper-reference-buffers 'go-test-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (define-key go-mode-map (kbd "C-c C-c C-f") 'go-test-current-file)
                            (define-key go-mode-map (kbd "C-C C-C C-c") 'go-test-current-test)
                            (define-key go-mode-map (kbd "C-c C-c C-t") 'go-test-current-project)
                            ))
  )

(require 'werkwright-lsp)
(require 'lsp-go)
(setq company-backends '((company-capf company-yasnippet)))
(use-package company
  :config
  (push 'company-capf company-backends)
  (push 'company-yasnippet company-backends)
  )

(setq flycheck-go-staticcheck-executable "/home/msteger/work/cthulhu/docode/bin/staticcheck")
(setq flycheck-go-errcheck-executable "/home/msteger/work/cthulhu/docode/bin/errcheck")

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
                               (lsp-deferred)
                               ))
                           (local-set-key (kbd "M-.") #'lsp-find-definition)
                           (rainbow-delimiters-mode)
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

(setq flycheck-go-golint-executable "/home/msteger/work/cthulhu/docode/bin/golint")
(setq lsp-gopls-server-path "/home/msteger/work/cthulhu/docode/bin/gopls")

(setq flycheck-error-list-format `[("Line" 20 flycheck-error-list-entry-< :right-align t)
    ("Col" 10 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-level-<)
    ("ID" 20 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)])

(provide 'werkwright-go)

