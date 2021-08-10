(use-package lsp-mode
  :config
  (setq lsp-ui-doc-enable nil) 
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)

  ;; optionally if you want to use debugger
  (setq lsp-eldoc-render-all nil)
)

(provide 'werkwright-lsp)
