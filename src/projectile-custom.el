;; Projectile customization using other proj features. Should be loaded last

(require 'projectile)

(define-key projectile-mode-map (kbd "M-j") 'projectile-command-map)

(define-key projectile-mode-map (kbd "C-c C-t p") 'projectile-test-project)
(define-key projectile-mode-map (kbd "C-c C-r") 'projectile-run-project)
(define-key projectile-mode-map (kbd "M-j s") 'projectile-grep)
(define-key projectile-mode-map (kbd "C-c C-z") 'proj-open-elisp-toplevel)
(define-key projectile-mode-map (kbd "C-x p") 'proj-open-project-file)

(setq projectile-completion-system 'ivy)

(setq projectile-project-search-path '("~/drafts/" "~/sources/"))

(setq projectile-switch-project-action #'(lambda () (proj-open-pfile)))

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
