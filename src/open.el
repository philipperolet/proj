;;; All functions for custom project opening.
;;; Main entry point is proj-open
(load "src/utils")

;; Files to be considered as ``project`` files, in order of most projecty
(defconst proj--project-files '("project.md" "README.md"))

;; Regexp excluding emacs temp files and hidden files
(defconst remove-unwanted-files-regexp "^[^\.].*[^~#]$")

(setq proj--already-opened-projects ())

(defun proj-open ()
  "opens projects, executing various actions depending on whether
  project has already been opened in this session"
  (if (member (projectile-project-name) proj--already-opened-projects)
      (proj-open-relevant (projectile-project-root))
    (add-to-list 'proj--already-opened-projects (projectile-project-name))
    (message "opening for the 1st time %s" (projectile-project-name))
    (proj-open-pfile)))
  
(defun proj-open-relevant (path)
  "Displays relevant project files according to a logic described
  in proj--get-relevant-files)"
  (let ((files (proj--get-relevant-files (proj--dir-files-and-attrs-recursive
				    path
				    remove-unwanted-files-regexp))))
    (delete-other-windows)
    (find-file (car files))
    (if (cdr files) (find-file-other-window (cadr files)))
    ;; add project dir to load-path
    (let ((default-directory path))
      (normal-top-level-add-to-load-path '(".")))
    (other-window 1)))

(defun proj-open-pfile ()
  "Displays the project file & magit"
  (delete-other-windows)
  (proj-open-project-file)
  (magit-status)
  (other-window -1))

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
   a relevant project file"
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

(defun proj-open-project-file ()
  "Opens project.md in the leftmost window"
  (interactive)
  (other-window 1)
  (select-window (or (window-left-child (frame-root-window))
		     (frame-root-window)))
  (find-file (concat (projectile-project-root) "project.md")))

(defun proj-toggle-mosaic ()
  (interactive)
  (if (= (count-windows) 1)
      (progn
	(other-window 1)
	(split-window-right)
	(other-window 1)
	(next-buffer)
	(other-window -1))
    (neotree-hide)
    (delete-other-windows)))

