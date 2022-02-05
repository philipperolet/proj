;;;; A few project utils for emacs. See README.md for info.

;;; This is the main file providing feature proj, to add
;;; in .emacs

(load "src/proj-lisp")
(load "src/open")
(load "src/proj-clj")
(load "src/packages")
(load "src/projectile-custom")

(global-set-key (kbd "<C-dead-acute>") 'proj-toggle-mosaic)

(provide 'proj)
