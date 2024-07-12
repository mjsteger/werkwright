(use-package keepass
  :straight (:host gitlab :repo "tay-dev/keepass.el"))

(use-package popper
  :init
  (popper-echo-mode +1)
  (setq popper-display-control t)
  (setq popper-reference-buffers '("\\*Messages\\*$"
                                   go-test-mode
                                   (lambda (buf) (with-current-buffer buf
                                              (and (not (string-equal (buffer-name) "*Org Agenda*"))
                                                   (not (s-match ".*vterm.*" (buffer-name)))
                                                        (derived-mode-p 'fundamental-mode)
                                                   (< (count-lines (point-min) (point-max))
                                                      10))))
                                   ))
  (popper-mode +1)
  :config
  (setq popper-groups '())
  (defun popper-group-by-steggy ()
    "This function should return a string or symbol that is the
name of the group this buffer belongs to. It is called with each
popup buffer as current, so you can use buffer-local variables."
    (cond
     ((s-match "magit: .*" (buffer-name (current-buffer)))
      "magit")
     ((s-match "\\*popper-group\\*:\\(.*\\):.*" (buffer-name (current-buffer)))
      (cadr (s-match "\\*popper-group\\*:\\(.*\\):.*" (buffer-name (current-buffer)))))
     ((buffer-local-boundp (intern "popper-group-variable") (current-buffer))
      popper-group-variable)
     (t (popper-group-by-projectile))
     ))

  (defun set-popper-group (&optional arg)
    (interactive "P")
    (let ((pgroup (ivy-completing-read "What popper group: " popper-groups nil nil)))
      (add-to-list 'popper-groups pgroup)
      (setq-local popper-group-variable pgroup)))
  (setq popper-group-function #'popper-group-by-steggy)
  )

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq-default treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (python-mode . python-ts-mode)))

  (use-package tree-sitter-langs))

(use-package expreg
  :config
  (keymap-global-set "C-@" 'expreg-expand)
  )

;; Do the envrc thing for emacs buffers
(use-package envrc
  :init
  (envrc-global-mode))

;; gimme subword navigation
(global-subword-mode 1)

(provide 'werkwright-editor-tweaks)
