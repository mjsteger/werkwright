(use-package rustic
  :config
  ;; For guix
  (when (string= system-type "gnu/linux")
    (setq lsp-rust-analyzer-store-path (executable-find "rust-analyzer")))
  (setq rust-format-on-save t)
  (define-key rust-mode-map ";" (kbd "C-q ; RET"))
  (define-key rust-mode-map (kbd "C-j") 'lsp-rust-analyzer-join-lines)
  (define-key rust-mode-map (kbd "C-@") 'lsp-extend-selection)
  (define-key rust-mode-map (kbd "C-c C-c t") 'lsp-execute-code-action)
  ;; Per the docs
  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)
  (setq rustic-format-trigger 'on-compile)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-idle-delay 0))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "lldb"
         :target nil
         :cwd nil)))

(provide 'werkwright-rust)
