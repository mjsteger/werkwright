(use-package rustic
  :config
  ;; For guix
  (when (string= system-type "gnu/linux")
    (setq lsp-rust-analyzer-store-path (executable-find "rust-analyzer")))
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-j") 'lsp-rust-analyzer-join-lines)
  (define-key rust-mode-map (kbd "C-@") 'lsp-extend-selection)
  (setq rustic-format-trigger 'on-save)
  )

(provide 'werkwright-rust)
