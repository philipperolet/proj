;;;; Emacs lisp project functions

(defun proj--lisp-test-at-point ()
  (interactive)
  (save-buffer)
  (eval-buffer)
  (ert :unexpected))

(defun proj--lisp-load-and-test-all ()
  (interactive)
  (save-some-buffers)
  (proj--lisp-load-sources)
  (proj--lisp-load-tests)
  (ert t))

(defun proj--lisp-run-project ()
  (load "run"))

(defun proj--lisp-load-sources ()
  (seq-map #'load-file (directory-files-recursively
			(concat (cdr (project-current)) "src/")
			".*\.el$")))

(defun proj--lisp-load-tests ()
  (seq-map #'load-file (directory-files-recursively
			(concat (cdr (project-current)) "test/")
			".*\.el$")))


;;; Hooks in projectile custom project type definition
;;; require a function returning a function

(defun proj--lisp-load-and-test-all-projectile ()
  #'proj--lisp-load-and-test-all)

(defun proj--lisp-run-project-projectile ()
  #'proj--lisp-run-project)
