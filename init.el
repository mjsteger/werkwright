(defvar werkwright-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(when (version< emacs-version "29.0")
  (error "[Werkwright] Werkwright requires GNU Emacs 29 or newer, but you're running %s" emacs-version))

;; Added this nonsense because I was getting recursive loads from jka-compr
(setq load-prefer-newer nil)
(require 'jka-compr)

;; Always load newest byte code
(setq load-prefer-newer t)
;; Define Werkwright's directory structure
(defvar werkwright-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Werkwright distribution.")
(defvar werkwright-core-dir (expand-file-name "core" werkwright-dir)
  "The home of Werkwright's core functionality.")
(defvar werkwright-modules-dir (expand-file-name  "modules" werkwright-dir)
  "This directory houses all of the built-in Werkwright modules.")
(defvar werkwright-personal-dir (expand-file-name "personal" user-emacs-directory)
  "This directory is for your personal configuration. All Emacs Lisp files there
are loaded automatically by Werkwright.")
(defvar werkwright-personal-preload-dir (expand-file-name "preload" werkwright-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Werkwright.")
(defvar werkwright-personal-modules '()
  "Modules you want loaded in werkwright that are personal config, and don't make sense for others.")
(defvar werkwright-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar werkwright-modules-file (expand-file-name "werkwright-modules.el" werkwright-core-dir)
  "This file contains a list of modules that will be loaded by Werkwright.")

(unless (file-exists-p werkwright-savefile-dir)
  (make-directory werkwright-savefile-dir))

(defun werkwright-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (werkwright-add-subfolders-to-load-path name)))))

;; add Werkwright's directories to Emacs's `load-path'
(add-to-list 'load-path werkwright-core-dir)
(add-to-list 'load-path werkwright-modules-dir)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `werkwright-personal-preload-dir'
(when (file-directory-p werkwright-personal-preload-dir)
  (message "[Werkwright] Loading personal configuration files in %s..." werkwright-personal-preload-dir)
  (mapc 'load (directory-files werkwright-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Werkwright] Loading Werkwright's core modules...")

;; Include these so I can use fancy mapping in core stuff
(use-package dash)
(use-package s)

(--each '(
	  werkwright-packages
	  werkwright-ui
	  werkwright-core
	  werkwright-editor)
  (require it))


;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'werkwright-osx))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'werkwright-linux))

(message "[Werkwright] Loading Werkwright's additional modules...")

;; the modules
(if (file-exists-p werkwright-modules-file)
    (load werkwright-modules-file))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" werkwright-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p werkwright-personal-dir)
  (mapc 'load (delete
               werkwright-modules-file
               (directory-files werkwright-personal-dir 't "^[^#\.].*\\.el$"))))

;; Man do you want a server for emacsclient
(server-start)

(provide 'init)

;;; init.el ends here

