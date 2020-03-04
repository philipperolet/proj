;;; Open project

(defconst projects-paths '("~/drafts/" "~/sources/")) ; with trailing separator
(defconst relevant-project-files '("README.md" "project.md"))
(defconst dirsep "/")

(defconst remove-emacs-temp-files-regexp "[^~#]$")

(defun open-project (name)
  "Creates a project IDE, with neotree on the project directory &
  project files opened (emacs temp files are ignored)"
  (interactive (list (completing-read "Project Name: "
				      (get-dir-list-from-paths projects-paths)
				      nil t)))
  
  (let ((path (concat (get-path name) name)))
    (let ((files (get-relevant-files (dir-files-and-attrs-recursive
				      path
				      remove-emacs-temp-files-regexp))))
      (delete-other-windows)
      (find-file (car files))
      (if (cdr files) (find-file-other-window (cadr files)))
      ;; add project dir to load-path
      (let ((default-directory (concat path dirsep))) (normal-top-level-add-to-load-path '(".")))
      (other-window 1))))

(defun dir-files-and-attrs-recursive (path regexp)
  "Like directory files and attributes, but recursively search
  subdirs and does not include subdirs in result."
  (seq-map (lambda (file) (cons file (file-attributes file)))
	   (directory-files-recursively path regexp)))

(defun get-relevant-files (files-list)
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
    (let ((project-file (get-first-common-element relevant-project-files (mapcar #'car files-list)))
	  (latest-file-data (argmax files-list #'compare-files-by-modif-date)))
      (let ((2ndlatest-file-data
	     (argmax (remove latest-file-data files-list) #'compare-files-by-modif-date)))
	(compute-relevant-files project-file (car latest-file-data) (car 2ndlatest-file-data))))))

(defun compute-relevant-files (project-file recent-file recent-file2)
  (cond ((equal project-file recent-file) (list project-file recent-file2))
	((null project-file) (list recent-file recent-file2))
	(t (list recent-file project-file))))

(defun get-first-common-element (list1 list2)
  "Returns the first element of list1 that belongs to list2, or nil"
  (car (seq-filter (lambda (x) (member x list2)) list1)))

(defun compare-files-by-modif-date (f1 f2)
  "Compare file data in form of (filename filename-attributes)
  and returns the most recently modified"
  (cond
   ((not f1) f2)
   ((not f2) f1)
   ((time-less-p (file-attribute-modification-time (cdr f1)) (file-attribute-modification-time (cdr f2))) f2)
   (t f1)))

(defun get-path (name)
  "Retrives the path of directory `name` from projects-paths"
  (car (seq-filter #'(lambda (dir) (file-directory-p (concat dir name)))
		   projects-paths)))

(defun get-dir-list-from-paths (pathlist)
  "Returns a list of directories residing in multiple paths"
  (seq-mapcat 'get-dir-list pathlist))

(defun get-dir-list (path)
  "Returns list of directories in the given path, as pairs: (dirname path)"
  (mapcar #'(lambda (dir-data) (list (car dir-data) path))
	  (seq-filter #'is-dir (directory-files-and-attributes path))))
	  
;;; file-data as returned by directory-files-and-attributes: (filename attrs)
(defun is-dir (file-data) (cadr file-data))

(defun argmax (listt compare-func &optional acc)
  "Returns the largest element of the list according to
   compare-func. compare-func must return the largest of 2
   elements, handling nil values. acc is for internal use only."
  (if (null listt)
      acc
    (let ((current-max (funcall compare-func acc (car listt))))
      (argmax (cdr listt) compare-func current-max))))
