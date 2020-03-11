;;;; A few util functions for proj

(defun proj--dir-files-and-attrs-recursive (path regexp)
  "Like directory files and attributes, but recursively search
   subdirs and does not include subdirs in result.
   Also ignores hidden files and dirs."
  (seq-map (lambda (file) (cons file (file-attributes file)))
	   (seq-filter (lambda (file) (not (string-match "/\\.\\|^\\." file)))
		       (directory-files-recursively path regexp))))

(defun proj--get-first-common-element (list1 list2)
  "Returns the first element of list1 that belongs to list2, or nil"
  (car (seq-filter (lambda (x) (member x list2)) list1)))

(defun proj--compare-files-by-modif-date (f1 f2)
  "Compare file data in form of (filename filename-attributes)
  and returns the most recently modified"
  (cond
   ((not f1) f2)
   ((not f2) f1)
   ((time-less-p (file-attribute-modification-time (cdr f1)) (file-attribute-modification-time (cdr f2))) f2)
   (t f1)))

(defun proj--get-dir-list-from-paths (pathlist)
  "Returns a list of directories residing in multiple paths"
  (seq-mapcat 'proj--get-dir-list pathlist))

(defun proj--get-dir-list (path)
  "Returns list of directories in the given path, as pairs: (dirname path)"
  (mapcar #'(lambda (dir-data) (list (car dir-data) path))
	  (seq-filter #'proj--is-dir (directory-files-and-attributes path))))

;;; file-data as returned by directory-files-and-attributes: (filename attrs)
(defun proj--is-dir (file-data) (cadr file-data))
	  
(defun proj--argmax (listt compare-func &optional acc)
  "Returns the largest element of the list according to
   compare-func. compare-func must return the largest of 2
   elements, handling nil values. acc is for internal use only."
  (if (null listt)
      acc
    (let ((current-max (funcall compare-func acc (car listt))))
      (proj--argmax (cdr listt) compare-func current-max))))
