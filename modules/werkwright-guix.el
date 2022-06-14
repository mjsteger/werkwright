(use-package geiser)
(use-package geiser-guile)
(use-package guix)

(load-file "~/play/guix/etc/copyright.el")
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/play/guix"))

(provide 'werkwright-guix)
