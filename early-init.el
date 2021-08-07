;; Shamelessly stolen from https://raw.githubusercontent.com/meatcar/emacs.d/master/config.org , though likely with modification knowing me

; Helps speed up startup
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Following [[https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly][Doom-Emacs FAQ]], we max the garbage collection threshold on startup, and reset it to the original value after.

;; max memory available for gc on startup
(defvar me/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
;; Defer it so that commands launched immediately after will enjoy the
;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)
(setq garbage-collection-messages t)

;; We also set the ~file-name-handler-alist~ to an empty list, and reset it after Emacs has finished initializing.
(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)

;; Optimizations for improving I/O performance. Increase max bytes read from a sub-process in a single op (Emacs 27+)
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; [[https://github.com/raxod502/straight.el][straight.el]] is used to download packages for us from all over the web. It stores them all in their respective git folders in =.emacs.d/straight=, which makes debugging, and contributing fixes back upstream as easy as possible.

(setq straight-use-package-by-default t
      use-package-always-defer t
      straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      vc-follow-symlinks t)

;; Make sude that straight lets you debug on error 
(setq debug-on-error t)

(setq straight-disable-native-compile t)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Bootstrap straight to get use-package
(require 'straight-x)
(straight-use-package 'use-package)

;; We use [[https://github.com/jschaf/esup][esup]] and [[https://github.com/dholm/benchmark-init-el][benchmark-init-el]] to keep tabs on our startup speed.
(use-package esup
  :demand t
  :commands esup)

;; Print to messages on total startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format
             "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
            gcs-done)))

;; Garbage collector magic hack. Yes really
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

