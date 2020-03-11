;;; proj-open / proj-open-from-path, main functions to open a project

(load "src/utils")

(defconst projects-paths '("~/drafts/" "~/sources/")) ; with trailing separator
(defconst relevant-project-files '("README.md" "project.md"))
(defconst dirsep "/")

;; Regexp excluding emacs temp files and hidden files
(defconst remove-unwanted-files-regexp "^[^\.].*[^~#]$")

(defun proj-open (name)
  "Creates a project IDE, with neotree on the project directory &
  project files opened (emacs temp files are ignored)"
  (interactive (list (completing-read "Project Name: "
				      (proj--get-dir-list-from-paths projects-paths)
				      nil t)))
  
  (let ((path (concat (proj--get-project-path name) name)))
    (proj-open-from-path path)))

(defun proj-open-from-path (path)
  (let ((files (proj--get-relevant-files (proj--dir-files-and-attrs-recursive
				    path
				    remove-unwanted-files-regexp))))
    (delete-other-windows)
    (find-file (car files))
    (if (cdr files) (find-file-other-window (cadr files)))
    ;; add project dir to load-path
    (let ((default-directory (concat path dirsep))) (normal-top-level-add-to-load-path '(".")))
    (other-window 1)))
  
(defun proj--get-relevant-files (files-list)
  "Returns either one or two files most relevant to a project
  
  files-list is a list of (filename attributes) as returned by
  eg. (directory-files-and-attributes) 

  Most relevant files are the most recently edited file (first)
  and the project file (second). If no project file is found the
  2nd most recently edited file is returned instead. If the
  project file is the most recently edited, it comes first and
  the second most recently edited file is sent second."
  (if (null files-list)
      (list ".")
    (let ((project-file (proj--get-first-common-element relevant-project-files (mapcar #'car files-list)))
	  (latest-file-data (proj--argmax files-list #'proj--compare-files-by-modif-date)))
      (let ((2ndlatest-file-data
	     (proj--argmax (remove latest-file-data files-list) #'proj--compare-files-by-modif-date)))
	(proj--compute-relevant-files project-file (car latest-file-data) (car 2ndlatest-file-data))))))

(defun proj--compute-relevant-files (project-file recent-file recent-file2)
  (cond ((equal project-file recent-file) (list project-file recent-file2))
	((null project-file) (list recent-file recent-file2))
	(t (list recent-file project-file))))

(defun proj--get-project-path (name)
  "Retrives the path of directory `name` from projects-paths"
  (car (seq-filter #'(lambda (dir) (file-directory-p (concat dir name)))
		   projects-paths)))
