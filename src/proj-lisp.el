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

(defun proj--lisp-run-project (project-root)
  (load-file (concat (project-root) "run.el")))

(defun proj--lisp-load-sources ()
  (seq-map #'load-file (directory-files-recursively
			(concat (cdr (project-current)) "src/")
			".*\.el$")))

(defun proj--lisp-load-tests ()
  (seq-map #'load-file (directory-files-recursively
			(concat (cdr (project-current)) "test/")
			".*\.el$")))


(defun proj-open-elisp-toplevel ()
  "Opens scratch in a side window for easy lisp eval"
  (interactive)
  (if (= (count-windows) 1)
      (split-window-right)
    (select-window (frame-first-window)))
  (other-window 1)
  (switch-to-buffer "*scratch*"))

;;; Hooks in projectile custom project type definition
;;; require a function returning a function

(defun proj--lisp-load-and-test-all-projectile ()
  #'proj--lisp-load-and-test-all)

(defun proj--lisp-run-project-projectile ()
  (lambda () (proj--lisp-run-project (projectile-project-root))))

