;;; Open project

(defconst projects-paths '("~/drafts/" "~/sources/")) ; with trailing separator
(defconst relevant-project-files '("README.md" "project.md"))
(defconst dirsep "/")

(defun open-project (name)
  "Creates a project IDE, with neotree on the project directory & project files opened"
  (interactive (list (completing-read "Project Name: "
				      (get-dir-list-from-paths projects-paths)
				      nil t)))
  
  (let ((path (concat (get-path name) name)))
    (let ((files (get-relevant-files (directory-files-and-attributes path))))
      (delete-other-windows)
      (find-file (concat path dirsep (car files)))
      (if (cdr files) (find-file-other-window (concat path dirsep (cadr files))))
      (neotree-dir path) ; open browsing tree on the left
      (other-window 1))))

(defun get-relevant-files (files-list)
  "Returns either one or two files most relevant to a project
  
  files-list is a list of (filename attributes) as returned by
  eg. (directory-files-and-attributes) 

  Most relevant files are the most recently edited file (first)
  and the project file (second). If no project file is found the
  2nd most recently edited file is returned instead. If the
  project file is the most recently edited, it comes first and
  the second most recently edited file is sent second."
  (let ((no-dir-list (seq-filter (lambda (x) (not (is-dir x))) files-list)))
    (if (null no-dir-list)
	(list ".")
      (let ((project-file (get-first-common-element relevant-project-files (mapcar #'car no-dir-list)))
	    (latest-file-data (argmax no-dir-list #'compare-files-by-access-date)))
	(let ((2ndlatest-file-data
	       (argmax (remove latest-file-data no-dir-list) #'compare-files-by-access-date)))
	  (compute-relevant-files project-file (car latest-file-data) (car 2ndlatest-file-data)))))))

(defun compute-relevant-files (project-file recent-file recent-file2)
  (cond ((equal project-file recent-file) (list project-file recent-file2))
	((null project-file) (list recent-file recent-file2))
	(t (list recent-file project-file))))

(defun get-first-common-element (list1 list2)
  "Returns the first element of list1 that belongs to list2, or nil"
  (car (seq-filter (lambda (x) (member x list2)) list1)))

(defun compare-files-by-access-date (f1 f2)
  "Compare file data in form of (filename filename-attributes)
  and returns the most recently accessed"
  (cond
   ((not f1) f2)
   ((not f2) f1)
   ((time-less-p (file-attribute-access-time (cdr f1)) (file-attribute-access-time (cdr f2))) f2)
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
