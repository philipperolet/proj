;;; All functions for custom project opening.
;;; Main entry point is proj-open
(require 'subr-x)
(load "src/utils")
(load "src/actions-sequence")

;; Files to be considered as ``project`` files, in order of most projecty
(defconst proj--project-files '("project.md" "README.md"))

;; Regexp excluding emacs temp files and hidden files
(defconst remove-unwanted-files-regexp "^[^\.].*[^~#]$")

(setq proj--already-opened-projects ())

(setq proj--state (list :project-data nil
			:opened-projects ()))

(defun proj-open (project-data)
  "Opens projects, executing various actions depending on project. project-data
   is a plist with :root, :name and :type props"
  ;; sets up state
  (proj--set :project-data project-data)
  (proj--set :action-vars (proj--compute-all-action-vars))
  (proj--set :tags (proj--compute-project-tags))

  ;; runs opening actions
  (proj--render-opening-actions)

  ;; update opened projects
  (if (member :first-opened (proj--get :tags))
      (proj--set :opened-projects
		 (cons (proj--get :project-data :name) (proj--get :opened-projects)))))

(defun proj-open-project-file (project-file)
  "Opens the project file (i.e. the first available from
  proj--project-files) in the leftmost window"
  (interactive)
  (select-window (or (window-left-child (frame-root-window))
		     (frame-root-window)))
  (find-file project-file))


(defun proj--compute-project-tags ()
  "Computes the list of all tags applicable to the current project"
  (list
   ;; add project type to tags, converting symbol to keyword   
   (thread-last (proj--get :project-data :type)
     (symbol-name)
     (concat ":")
     (read))
   
   ;; add :first-opened or :already-opened depending on project
   (if (member (proj--get :project-data :name) (proj--get :opened-projects))
       :already-opened
     :first-opened)

  ;; add git or no-git tag
  (if (file-directory-p (concat (proj--get :project-data :root) ".git"))
      :git
    :no-git)))
       
(defun proj--compute-all-action-vars ()
  "Computes all action vars that are mentioned in the action sequence"
  (thread-last proj--actions-seq
    (mapcar 'caddr) ;; get list of args list of actions
    (apply #'append) ;; merge in a single list
    (delete-dups)
    (seq-filter #'keywordp) ;; action vars are keywords
    (mapcar (lambda (action-var) (list action-var (proj--compute-action-var action-var))))
    (apply #'append))) ;; finally merge into a plist

(defun proj--compute-action-var (arg)
  (let ((files-list (proj--dir-files-and-attrs-recursive
		     (proj--get :project-data :root)
		     remove-unwanted-files-regexp)))
    (cond
     ((eq arg :most-recent-file) (car (proj--get-relevant-files files-list)))
     ((eq arg :project-file) (car (proj--get-project-file files-list)))
     ((eq arg :2nd-most-relevant) (cadr (proj--get-relevant-files files-list)))
     (t (error "No action var %s. Check actions sequence." arg)))))

(defun proj--replace-action-vars-in-args (args)
  "For all args, either the arg is a keyword and should be
   replaced if it is an action var, or it is a regular arg and
   should stay the same"
  (mapcar
   (lambda (arg)
     (if (keywordp arg)
	 (plist-get (plist-get proj--state :action-vars) arg)
       arg))
   args))
  
(defun proj--render-opening-actions ()
  "Executes all actions whose tags are in project tags, replacing
   every action var by its value in the action's argument list."
  (dolist (action proj--actions-seq)
    (if (seq-every-p (lambda (elt) (member elt (plist-get proj--state :tags))) (car action))
	(apply (cadr action) (proj--replace-action-vars-in-args (caddr action))))))

(defun proj--add-to-path ()
  (let ((default-directory (proj--get :project-data :root)))
    (normal-top-level-add-to-load-path '("."))))

(defun proj--get-relevant-files (files-list)
  "Returns either one or two files most relevant to a project
  
  files-list is a list of (filename attributes) as returned by
  eg. (directory-files-and-attributes) 

  Most relevant files are the most recently edited file (first)
  and the project file (second). If multiple project files exist
  the first in order of proj--project-files list is chosen.
  If no project file is found the 2nd most recently edited file
  is returned instead. If the project file is the most recently
  edited, it comes second and the second most recently edited file
  is sent first."
  (if (null files-list)
      (list ".")
    (let ((project-file (proj--get-project-file files-list))
	  (latest-file-data (proj--argmax files-list #'proj--compare-files-by-modif-date)))
      (let ((2ndlatest-file-data
	     (proj--argmax (remove latest-file-data files-list) #'proj--compare-files-by-modif-date)))
	(proj--compute-relevant-files (car project-file) (car latest-file-data) (car 2ndlatest-file-data))))))

(defun proj--get-project-file (files-list)
  "Returns the first file (with attrs) in files-list whose name is
   a relevant project file, or nil if none is found."
  (let ((project-file-in-files-list
	 (lambda (project-file)
	   (seq-find
	    (lambda (file) (equal (file-name-nondirectory (car file)) project-file))
	    files-list))))
    (seq-some project-file-in-files-list proj--project-files)))
      
(defun proj--compute-relevant-files (project-file recent-file recent-file2)
  (cond ((equal project-file recent-file) (list recent-file2 project-file))
	((null project-file) (list recent-file recent-file2))
	(t (list recent-file project-file))))

(defun proj-toggle-mosaic ()
  (interactive)
  (if (= (count-windows) 1)
      (progn
	(other-window 1)
	(split-window-right)
	(other-window 1)
	(next-buffer)
	(other-window -1))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (next-buffer)
    (other-window -1)))
