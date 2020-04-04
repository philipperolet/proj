(load "src/open")
(load "test/mocks")

;;; proj--get-relevant-files

(ert-deftest proj--get-relevant-files ()

  ;; if there are files, return the most recent first and the project file then
  (let ((mock-files (list mock-oldfile mock-newfile mock-projfile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/newfile" "/project/path/project.md"))))

  ;; README.md is a valid project file
  (let ((mock-files (list mock-oldfile mock-newfile mock-readmefile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/newfile" "/project/path/README.md"))))

  ;; If a project file is most recent, return it second, and the second most recent first
  (let ((mock-files (list mock-olderfile mock-readmefile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/olderfile" "/project/path/README.md"))))
  (let ((mock-files (list mock-oldfile mock-projfile mock-olderfile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/oldfile" "/project/path/project.md"))))

  ;; If multiple project files are there, favor the "most projecty" one (1st in list)
  (let ((mock-files (list mock-oldfile mock-newfile mock-projfile mock-readmefile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/newfile" "/project/path/project.md"))))
  (let ((mock-files (list mock-oldfile mock-newfile mock-readmefile mock-projfile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/newfile" "/project/path/project.md"))))
  
  ;; If there are no project file, return only the latest files
  (let ((mock-files (list mock-oldfile mock-newfile mock-olderfile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/newfile" "/project/path/oldfile"))))
  
  ;; If there are more than 2 files more recent than project file, still return project
  ;; file in second
  (let ((mock-files (list mock-newerfile mock-projfile mock-newfile mock-olderfile)))
    (should (equal (proj--get-relevant-files mock-files)
		   '("/project/path/newerfile" "/project/path/project.md")))))
  
(ert-deftest valid-emacs-regexp ()
  ;;; checks the regexp for emacs temp & hidden files work
  (let ((test-list '("/file1.md" "/home/file2" "file3~" "/#file4" "/truc/#file6.el#" ".file7")))
    (should (equal
	     (seq-filter (lambda (s) (string-match-p remove-unwanted-files-regexp s)) test-list)
	     '("/file1.md" "/home/file2" "/#file4")))))

(ert-deftest proj--get-project-file-- ()
  (let ((mock-files (list mock-oldfile mock-newfile mock-projfile mock-readmefile)))
    (should (equal (proj--get-project-file mock-files) mock-projfile))
    (should (equal (proj--get-project-file (list mock-oldfile mock-newfile)) nil))))

(defun proj-mockfn (&rest args)
  "Mock function used to test project action execution. When run,
   it just adds the list of args with which it was executed in
   proj--execution-trace. Then checking execution went correctly
   amounts to comparing proj--execution-trace with what was
   expected. Note: proj--execution-trace is not defined and must
   be in every test where proj--mockfn is used"
  (add-to-list 'proj--execution-trace args))

(ert-deftest proj-open--simple-action-list ()
  ;;; When given a simple action list, runs the given actions
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (1)))))
    (proj-open '(:root "/my-root/" :name "pname" :type my-type))
    (should (equal proj--execution-trace '((1)))))
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (1 2)) '(nil proj-mockfn (3 4 5)))))
    (proj-open '(:root "/my-root/" :name "pname" :type my-type))
    (should (equal proj--execution-trace '((3 4 5) (1 2))))))

(ert-deftest proj-open--relevant-files-keyword ()
  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '(nil proj-mockfn (:most-recent-file))
				     '(nil proj-mockfn (:project-file))))
	    ((symbol-function 'proj--dir-files-and-attrs-recursive)
	     (lambda (p r) (list mock-oldfile mock-newfile mock-readmefile mock-projfile))))
    (proj-open '(:root "/my-root/" :name "pname" :type my-type))
    (should (equal proj--execution-trace
		     '(("/project/path/project.md") ("/project/path/newfile")))))

  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '(nil proj-mockfn (:most-recent-file :project-file))))
	    ((symbol-function 'proj--dir-files-and-attrs-recursive)
	     (lambda (p r) (list mock-readmefile mock-oldfile))))
    (proj-open '(:root "/my/2ndroot/" :name "pname" :type t))
    (should (equal proj--execution-trace
		   '(("/project/path/oldfile" "/project/path/README.md")))))

  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '(nil proj-mockfn (:2nd-most-relevant))))
	    ((symbol-function 'proj--dir-files-and-attrs-recursive)
	     (lambda (p r) (list mock-oldfile mock-newfile mock-readmefile mock-projfile))))
    (proj-open '(:root "/my/2ndroot/" :name "pname" :type t))
    (should (equal proj--execution-trace
		   '(("/project/path/project.md")))))
  
  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '(nil proj-mockfn (:2nd-most-relevant))))
	    ((symbol-function 'proj--dir-files-and-attrs-recursive)
	     (lambda (p r) (list mock-oldfile mock-newfile))))
    (proj-open '(:root "/my/2ndroot/" :name "pname" :type t))
    (should (equal proj--execution-trace
		   '(("/project/path/oldfile"))))))

(ert-deftest proj-open--undefined-keyword ()
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (:undefined-keyword)))))
    (should-error (proj-open '(:root "/yo/" :name "n" :type t)))))
  
(ert-deftest proj--compute-all-action-vars--test ()
  (cl-letf ((proj--execution-trace nil)
	    ((symbol-function 'proj--dir-files-and-attrs-recursive)
	     (lambda (p r) (list mock-oldfile mock-newfile)))
	    ((symbol-function 'proj--compute-action-var)
	     (lambda (arg) ()))
	    (proj--actions-seq (list '(nil proj-mockfn (a b c))
				     '(nil proj-mockfn (a :k1 :k2))
				     '(nil proj-mockfn (:k3))
				     '(nil proj-mockfn (:k2 3 :k3 :k4)))))
    (should (equal (proj--compute-all-action-vars)
		   '(:k1 nil :k2 nil :k3 nil :k4 nil)))))

(ert-deftest proj-open--lein-test-tag ()
  ;; should execute action list on lein-test project type but not on
  ;; other kind of project type
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (a b c))
				 '((:lein-test) proj-mockfn (1 2)))))
    (proj-open '(:root "/" :name "lol" :type lein-test))
    (should (equal proj--execution-trace
		   '((1 2) (a b c)))))
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (a b c))
				 '((:lein-test) proj-mockfn (1 2)))))
    (proj-open '(:root "/" :name "lol" :type blob))
    (should (equal proj--execution-trace
		   '((a b c))))))

(ert-deftest proj-open--undefined-tag ()
  ;; for undefined tags, actions should not execute. But they should for empty lists
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (a b c))
				 '((:hoo) proj-mockfn (1 2)))))
    (proj-open '(:root "/" :name "lol" :type blob))
    (should (equal proj--execution-trace
		   '((a b c))))))

(ert-deftest proj-open--first-opened-already-opened ()
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '((:first-opened) proj-mockfn (a b c))
				 '((:already-opened) proj-mockfn (1 2)))))
    (proj-open '(:root "/" :name "lol" :type blob))
    (should (equal proj--execution-trace
		   '((a b c))))
    (proj-open '(:root "/" :name "lol" :type blob))
    (should (equal proj--execution-trace
		   '((1 2) (a b c))))))
  
(ert-deftest git-tag ()
  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '((:git) proj-mockfn ("git"))
				     '((:no-git) proj-mockfn ("no-git"))))
	    ((symbol-function 'file-directory-p)
	     (lambda (file) (if (equal file "/root/p1/.git") t nil))))
    (proj-open '(:root "/root/p1/" :name "git-project" :type blob))
    (should (equal proj--execution-trace
		   '(("git"))))
    (proj-open '(:root "/root/p2/" :name "no-git-project" :type blob))
    (should (equal proj--execution-trace
		   '(("no-git") ("git"))))))

(ert-deftest nil-action-var ()
  ;; should err if action var does not exist, but work if it does exist but resolves to nil
  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '((:git) proj-mockfn (:no-such-action-var)))))
    (should-error (proj-open '(:root "/root/2/" :name "proj" :type blob))))
  (cl-letf ((proj--execution-trace nil)
	    (proj--actions-seq (list '((:first-opened) proj-mockfn (:project-file))))
  	    ((symbol-function 'proj--dir-files-and-attrs-recursive) (lambda (var var) ())))
    (proj-open '(:root "/root/p2/" :name "proj" :type blob))
    (should (equal proj--execution-trace
		   '((nil))))))
