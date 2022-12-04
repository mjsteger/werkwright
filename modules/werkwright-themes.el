(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-face-attribute 'mode-line-inactive nil :background "#323232")

;; Need to do this or you can't see whitespace
(set-face-attribute 'whitespace-tab nil :background "#2c2f3a")
(set-face-attribute 'whitespace-space nil :background "#2c2f3a")

(load-theme 'doom-vibrant t)

(provide 'werkwright-themes)
