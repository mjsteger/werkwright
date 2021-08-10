(defvar werkwright-modules '(
                             projectile
                             lsp
                             ace
                             go
                             efuncs
                             org
                             themes
                             ))

(mapc #'require
      (->> werkwright-modules
           ;; sequence ele to string
           (-map 'symbol-name)
           ;; add the naming to it
           (--map (concat "werkwright-" it))
           ;; back to symbol for require
           (-map 'intern)))
