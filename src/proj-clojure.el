;;; Clojure project functions

(defun proj-clj-autojackin ()
  "Runs cider-jack-in if in a leiningen project. Intended to be hooked to clojure files"
  (if (and (equal (projectile-project-type) 'lein-test)
	   (not (cider-repls)))
      (progn
	(cider-jack-in nil)
	(cider-test-run-project-tests nil))))
