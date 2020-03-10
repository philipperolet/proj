;;;; A few project utils for emacs. See README.md for info.

;;; This is the main file providing feature proj, to add
;;; in .emacs

(load "src/lisp-custom")
(load "src/open-project")

;; Global key map deactivated for portability
;; (global-set-key (kbd "C-x p") 'open-project)

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()  
   (local-set-key (kbd "C-c C-t") ;  binding to save and run the test at point
		  #'proj--lisp-test-at-point)
   (local-set-key (kbd "C-c C-c") ;  binding to save buffers & run all tests
		  #'proj--lisp-load-and-test-all)))

(provide 'proj)
