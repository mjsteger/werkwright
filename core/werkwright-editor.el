(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(straight-use-package 'smartparens)

;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)

(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode +1)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; diminish keeps the modeline tidy
(use-package diminish)
(require 'diminish)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" werkwright-savefile-dir))
;; activate it for all buffers
(save-place-mode 1)

;; savehist keeps track of some history
(use-package savehist)

(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" werkwright-savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" werkwright-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun werkwright-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list werkwright-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'werkwright-recentf-exclude-p)

(recentf-mode +1)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch

(use-package super-save)
(require 'super-save)
;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)
(diminish 'super-save-mode)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)


;; tramp, for sudo access
(require 'tramp)
(setq tramp-default-method "ssh")


;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defcustom werkwright-whitespace t
  "Non-nil values enable Werkwright's whitespace visualization."
  :type 'boolean
  :group 'werkwright)

(defcustom werkwright-flyspell t
  "Non-nil values enable Werkwright's flyspell support."
  :type 'boolean
  :group 'werkwright)

(defun werkwright-enable-flyspell ()
  "Enable command `flyspell-mode' if `werkwright-flyspell' is not nil."
  (when (and werkwright-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(defun werkwright-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `werkwright-clean-whitespace-on-save' is not nil."
  (when werkwright-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun werkwright-enable-whitespace ()
  "Enable `whitespace-mode' if `werkwright-whitespace' is not nil."
  (when werkwright-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'werkwright-cleanup-maybe nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'werkwright-enable-flyspell)
(add-hook 'text-mode-hook 'werkwright-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" werkwright-savefile-dir)
      bookmark-save-flag 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" werkwright-savefile-dir))
(projectile-mode t)

;; avy allows us to effectively navigate to visible things
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(use-package browse-kill-ring)
;; (browse-kill-ring-default-keybindings)
;; (global-set-key (kbd "s-y") 'browse-kill-ring)

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))


;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(use-package whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" werkwright-savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" werkwright-savefile-dir))

;; Compilation from Emacs
(defun werkwright-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'werkwright-colorize-compilation-buffer)

;; supercharge your undo/redo with undo-tree
(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)
(global-set-key (kbd "M-[") 'winner-undo)
(global-set-key (kbd "M-]") 'winner-redo)
(use-package rainbow-delimiters
  (rainbow-delimiters-mode +1))

;; diff-hl
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

;; use settings from .editorconfig file when present
(require 'editorconfig)
(editorconfig-mode 1)
(diminish 'editorconfig-mode)


(use-package prescient
  :defer 0.1
  :config
  (prescient-persist-mode 1))

(use-package ivy
  :defer 0.1
  :custom
  ;; add bookmarks and recentf to buffer lists
  (ivy-use-virtual-buffers t)
  ;; better matching method
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1))

(use-package counsel
  :defer 0.1
  :config
  (counsel-mode 1))

;; better fuzzy matching.
(use-package flx
  :defer 0.1
  :after ivy counsel)

(use-package ivy-prescient
  :defer 0.1
  :after ivy counsel prescient
  :config
  (ivy-prescient-mode 1))

;; add more information to ivy/counsel
(use-package ivy-rich
  :defer 0.1
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)

  :after ivy counsel all-the-icons-ivy-rich
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-posframe
  :defer 0.1
  :after ivy counsel
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-point)))
  (ivy-posframe-mode 1))

;; [[http://company-mode.github.io/][company-mode]] gives us the standard dropdown as-you-type of modern IDEs.
(use-package company
  :config
  (global-company-mode 1))

(use-package company-prescient
  :after company prescient
  :config
  (company-prescient-mode 1))

(use-package company-posframe
  :after company
  :config
  (company-posframe-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package flycheck-posframe
  :after flycheck company
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)
  (advice-add 'org-edit-src-exit :after #'flycheck-posframe-hide-posframe))

;; [[https://github.com/lassik/emacs-format-all-the-code][emacs-format-all-the-code]] knows about all the different formatters for different languuages, and tries to run them if they are installed. We configure it to format all modes that are in the ~auto-format-modes~ list on save. We well add modes to this later.
(defcustom auto-format-modes '()
  "Modes to turn on format-all-mode in")
(defcustom auto-format-dirs '()
  "Directories to turn on format-all-mode in")

(defun me/auto-format-buffer-p ()
  (and
   (member major-mode auto-format-modes)
   (buffer-file-name)
   (save-match-data
     (let ((dir (file-name-directory (buffer-file-name))))
       (cl-some (lambda (regexp) (string-match regexp dir))
                auto-format-dirs)))))

(defun me/maybe-format-all-mode ()
  (format-all-mode (if (me/auto-format-buffer-p) 1 0)))

(use-package format-all
  :commands format-all-mode
  :hook (after-change-major-mode . me/maybe-format-all-mode))

(use-package vterm)

;; For the love of god - when you type something w/ a mark, delete that
(delete-selection-mode +1)




;; [[https://github.com/iqbalansari/restart-emacs][restart-emacs]] teaches Emacs to restart itself. I added a ~me/reload-init~ command as well to just reload the =init.el= file without a full restart.

(defun me/reload-init ()
  "Reload init.el."
  (interactive)
  (message "Reloading init.el...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init.el... done."))

(use-package restart-emacs
  :commands restart-emacs
  )


(provide 'werkwright-editor)
