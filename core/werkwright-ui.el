(use-package better-defaults)

(setq 
      ;; always follow symlinks when opening files
      vc-follow-symlinks t
      ;; overwrite text when selected, like we expect.
      delete-seleciton-mode t
      ;; quiet startup
      inhibit-startup-message t
      initial-scratch-message nil
      ;; hopefully all themes we install are safe
      custom-safe-themes t
      ;; simple lock/backup file management
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      ;; when quiting emacs, just kill processes
      confirm-kill-processes nil
      ;; ask if local variables are safe once.
      enable-local-variables t)


;; use human-readable sizes in dired
(setq-default dired-listing-switches "-alh")

;; life is too short to type yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; always highlight code
(global-font-lock-mode 1)

;; Emacs is very conservative about assuming encoding. Everything is utf-8 these days, lets have that as the default.

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

(use-package no-littering
  :demand t
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))


;; [[https://github.com/justbur/emacs-which-key][which-key]] pops up a nice window whenever we hesitate about a keyboard shortcut, and shows all the possible keys we can press. Popularized by Spacemacs and Doom-Emacs, we can now configure absurd key combinations, forget about them, and then be delighted to discover them again!

(use-package which-key
  :demand t
  :custom
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (set-face-attribute
    'which-key-local-map-description-face nil :weight 'bold))


;; (setq ring-bell-function 'ignore ; no bell
;;       ;; better scrolling
;;       scroll-step 1
;;       scroll-conservatively 101
;;       scroll-preserve-screen-position 1
;;       mouse-wheel-scroll-amount '(1 ((shift) . 5))
;;       mouse-wheel-follow-mouse t
;;       ;; lines between the cursor and the edge of the screen
;;       scroll-margin 3

;;       ;; wrap lines that are too long.
;;       truncate-lines nil
;;       ;; don't resize frames a character at a time, but use pixels
;;       frame-resize-pixelwise t)



(use-package anzu
  :defer 1
  :after isearch
  :config
  (global-anzu-mode 1))

;; Always redraw immediately when scrolling, more responsive and doesn't hang! Sourced from http://emacs.stackexchange.com/a/31427/2418
(setq fast-but-imprecise-scrolling t
      jit-lock-defer-time 0)

;; [[https://github.com/ahungry/fast-scroll][fast-scroll]] "works by temporarily disabling font-lock and switching to a barebones mode-line, until you stop scrolling (at which point it re-enables)". It only does this when scrolling super fast, to keep everything responsive.
(use-package fast-scroll
  :defer 1
  :hook
  (fast-scroll-start . (lambda () (flycheck-mode -1)))
  (fast-scroll-end . (lambda () (flycheck-mode 1)))
  :config
  (fast-scroll-config)
  (fast-scroll-mode 1))

;; Ligatures
(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :defer 1
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures
   'prog-mode
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))
  (global-ligature-mode t))

;; [[https://github.com/hlissner/emacs-solaire-mode][solaire-mode]] darkens non-important buffers, to help you focus on what matters.
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; add some space between lines for easier reading.
(setq-default line-spacing 1)

;; highlight the current line
(global-hl-line-mode t)

;; Add padding inside buffer windows
(setq-default left-margin-width 2
              right-margin-width 2)
(set-window-buffer nil (current-buffer)) ; Use them now.

;; Add padding inside frames (windows)
(add-to-list 'default-frame-alist '(internal-border-width . 8))
(set-frame-parameter nil 'internal-border-width 8) ; Use them now

;; fix color display when loading emacs in terminal
(defun enable-256color-term ()
  (interactive)
  (load-library "term/xterm")
  (terminal-init-xterm))

(unless (display-graphic-p)
  (if (string-suffix-p "256color" (getenv "TERM"))
    (enable-256color-term)))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)
(use-package vivid-theme
  :straight (:host github :repo "websymphony/vivid-theme"))


(use-package smart-mode-line
  :init
  (setq sml/theme 'respectful)
  (setq sml/mode-width 1)
  (line-number-mode t)
  (column-number-mode t)
  (display-battery-mode +1)
  (setq sml/show-time nil)
  (setq sml/position-percentage-format nil)  
  (setq rm-whitelist ".*lsp.*")
  (setq display-time-default-load-average nil)
  (smart-mode-line-enable)
  (sml/apply-theme 'respectful)
  )

;; enable y/n 
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Werkwright - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))
(provide 'werkwright-ui)
