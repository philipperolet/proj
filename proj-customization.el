;;;; Customization for .emacs
(load "~/sources/proj/proj.el")

(global-set-key (kbd "C-x p") 'open-project)
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()  
   (eval-buffer)
   (local-set-key (kbd "C-c C-t") ;  binding to save and run the test at point
		  (lambda () 
		    (interactive)
		    (save-buffer)
		    (eval-defun nil)
		    (ert :new)))
   (local-set-key (kbd "C-c C-c") ;  binding to save buffer & run all tests
		  (lambda ()
		    (interactive)
		    (save-buffer)
		    (eval-buffer)
		    (ert t)))))
