(use-package projectile
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ; Sane defaults for rails
  (add-to-list 'projectile-globally-ignored-directories "log")
  (add-to-list 'projectile-globally-ignored-directories "tmp")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "uploads"
  )
)

(provide 'werkwright-projectile)
