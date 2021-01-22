;; Projectile customization using other proj features. Should be loaded last

(require 'projectile)

(define-key projectile-mode-map (kbd "M-j") 'projectile-command-map)

(define-key projectile-mode-map (kbd "C-c C-t p") 'projectile-test-project)
(define-key projectile-mode-map (kbd "C-c C-r") 'projectile-run-project)
(define-key projectile-mode-map (kbd "M-j s") 'projectile-grep)
(define-key projectile-mode-map (kbd "C-c C-z") 'proj-open-elisp-toplevel)

(defun proj-current-project-file ()
  (car (proj--get-project-file
	 (proj--dir-files-and-attrs-recursive
	  (projectile-project-root)
	  remove-unwanted-files-regexp))))
  
(define-key projectile-mode-map (kbd "C-x p")
  (lambda ()
    (interactive)
    (find-file (proj-current-project-file))))

(define-key projectile-mode-map (kbd "C-x 4 p")
  (lambda ()
    (interactive)
    (let ((project-file (proj-current-project-file)))
      (other-window 1)
      (find-file project-file))))

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

;; Tab line and buffer switching
(defun proj--tab-line-tabs-function ()
  "Returns file buffer of current project, or if no project, all file buffers."
  (seq-sort-by 'buffer-name 'string-lessp
	       (seq-filter 'buffer-file-name
			   (condition-case nil
			       (projectile-project-buffers)
			     (error (buffer-list))))))

(defun proj-cycle-buffers (offset)
  "Cycles through acceptable buffers, switches to the buffer at
position `current + offset` where offset is a pos or neg int."
  (let* ((buffers (proj--tab-line-tabs-function))
	(current-index (seq-position buffers (current-buffer))))
    (switch-to-buffer
     (seq-elt buffers (mod (+ current-index offset) (length buffers))))))

(global-set-key (kbd "<C-tab>")
		(lambda ()
		  (interactive)
		  (proj-cycle-buffers 1)))

(global-set-key (kbd "<C-dead-grave>")
		(lambda ()
		  (interactive)
		  (proj-cycle-buffers -1)))

;; Display internal buffers on right
(defun proj-display-buffer-right (buffer alist)
  (if (= (count-windows) 1)
      (window--display-buffer buffer (split-window-right) 'window alist)
    (window--display-buffer buffer
			   (window-in-direction 'right (frame-first-window))
			   'reuse
			   alist)))

(defun buffer-displayed-right-p (buffer-name action)
  (let ((excluded-buffers '("*Completions*")))
    (cond
     ((member buffer-name excluded-buffers) nil)
     ((string-prefix-p "*" buffer-name) t)
     ((string-prefix-p "magit: " buffer-name) t))))

(setq display-buffer-alist
      `((buffer-displayed-right-p proj-display-buffer-right
	 (direction . right) (window . root)
	 (window-parameters . ()))))
