;;;; A few util functions for proj
(defun proj--dir-files-and-attrs-recursive (path regexp)
  "Like directory files and attributes, but recursively search
   subdirs and does not include subdirs in result.
   Also ignores hidden files and dirs."
  (seq-map (lambda (file) (cons file (file-attributes file)))
	   (seq-filter (lambda (file) (not (string-match "/\\.\\|^\\." file)))
		       (directory-files-recursively path regexp nil
						    (lambda (dir)
						      (and (not (string-match "node_modules$" dir))
							   (not (string-match "/.git$" dir))))))))

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
	  
(defun proj--argmax (listt compare-func)
  "Returns the largest element of the list according to
   compare-func. compare-func must return the largest of 2
   elements, handling nil values"
  (seq-reduce compare-func listt nil))

(defun proj--get (prop &rest prop-chain)
  "Return the value of the given prop (or prop chain for nested
   properties) in proj--state"
  (seq-reduce
   (lambda (plist prop) (plist-get plist prop))
   (cons prop prop-chain)
   proj--state))

(defun proj--set (prop prop-or-val &optional val)
  "Sets the value of the given prop in proj--state. Optionally,
  sets the value of prop-or-val if it is a nested property of the
  prop plist"
  (if (not val)
      (setq proj--state (plist-put proj--state prop prop-or-val))
    (setq proj--state
	  (plist-put proj--state prop (plist-put (proj--get prop) prop-or-val val)))))

(defun proj--xah-convert-file-coding-system (@fpath @coding-system)
  "Convert file's encoding.
 *fpath is full path to file.
 *coding-system is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2015-07-24"
  (let ($buffer
        ($bufferOpened-p (get-file-buffer @fpath)))
    (if $bufferOpened-p
        (with-current-buffer $bufferOpened-p
          (set-buffer-file-coding-system @coding-system)
          (save-buffer))
      (progn
        (setq $buffer (find-file @fpath))
        (set-buffer-file-coding-system @coding-system)
        (save-buffer)
        (kill-buffer $buffer)))))
