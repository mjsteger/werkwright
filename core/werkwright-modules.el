(defvar werkwright-modules '(
                             projectile
                             lsp
                             ace
                             go
                             efuncs
                             org
                             themes
                             exwm
                             smudge
                             guix
                             terraform
                             lisp
                             rust
                             editor-tweaks
                             ))

(-concat werkwright-modules werkwright-personal-modules)

(mapc #'require
      (->> werkwright-modules
           ;; sequence ele to string
           (-map 'symbol-name)
           ;; add the naming to it
           (--map (concat "werkwright-" it))
           ;; back to symbol for require
           (-map 'intern)))


;; Remember you can always remove things from straight if they get into a bad state with
;; (remhash "rust-mode" straight--recipe-cache)
