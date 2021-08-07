;; (defun require-all-werkwright (requirelist)
;;   (require-all 
;;    (->> requirelist 
;;         (-map 'symbol-name) 
;;         (--map (concat "werkwright-" it)) 
;;         (-map 'intern))))

;; (werkwright-require-packages '(use-package))

;; (require-all-werkwright (append werkwright-personal-modules '(
;;                                                           ace
;;                                                           autocomplete
;;                                                           coffee
;;                                                           columns
;;                                                           dash
;;                                                           dired
;;                                                           gist
;;                                                           haml
;;                                                           ido
;;                                                           kill-ring
;;                                                           projectile
;;                                                           ruby
;;                                                           yasnippet
;;                                                           vagrant
;;                                                           lsp
;;                                                           go
;;                                                           javascript
;;                                                           html
;;                                                           )))
