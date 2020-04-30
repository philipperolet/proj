;; Projectile customization using other proj features. Should be loaded last

(require 'projectile)

(define-key projectile-mode-map (kbd "M-j") 'projectile-command-map)

(define-key projectile-mode-map (kbd "C-c C-t p") 'projectile-test-project)
(define-key projectile-mode-map (kbd "C-c C-r") 'projectile-run-project)
(define-key projectile-mode-map (kbd "M-j s") 'projectile-grep)
(define-key projectile-mode-map (kbd "C-c C-z") 'proj-open-elisp-toplevel)
(define-key projectile-mode-map (kbd "C-x p")
  (lambda () (interactive) (proj-open-project-file
			    (car (proj--get-project-file
				  (proj--dir-files-and-attrs-recursive
				   (projectile-project-root)
				   remove-unwanted-files-regexp))))))

(setq projectile-completion-system 'ivy)

(setq projectile-use-git-grep t)

(setq projectile-project-search-path '("~/drafts/" "~/sources/"))

(setq projectile-switch-project-action
      #'(lambda () (proj-open (list :root (projectile-project-root)
				    :name (projectile-project-name)
				    :type (projectile-project-type)))))

(projectile-register-project-type 'elisp '("run.el")
				  :src-dir "src/"
				  :test #'proj--lisp-load-and-test-all-projectile
				  :test-dir "test/"
				  :test-prefix "test-"
				  :run #'proj--lisp-run-project-projectile)

;; for clojure leiningen projects, redefine test command
(defun proj-run-cider-tests () (cider-test-run-project-tests nil))
(plist-put (alist-get 'lein-test projectile-project-types)
	   'test-command
	   'proj-run-cider-tests)

(projectile-mode +1)
