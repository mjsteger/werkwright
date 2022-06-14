(use-package lsp-java
  :config
  (require 'lsp-java)
  (add-hook 'java-mode-hook #'lsp)
  )


(provide 'werkwright-java)
;;; werkwright-java.el ends here
