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
    (should (equal (proj--get-project-file mock-files) mock-projfile))))

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
    (proj-open-new '(:root "/my-root/" :name "pname" :type "my-type"))
    (should (equal proj--execution-trace '((1)))))
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (1 2)) '(nil proj-mockfn (3 4 5)))))
    (proj-open-new '(:root "/my-root/" :name "pname" :type "my-type"))
    (should (equal proj--execution-trace '((3 4 5) (1 2))))))

(ert-deftest proj-open--relevant-files-keyword ()
  (let ((proj--execution-trace nil)
	(proj--actions-seq (list '(nil proj-mockfn (:relevant-files)))))
    (proj-open-new '(:root "/my-root/" :name "pname" :type "my-type"))
    (should (equal proj--execution-trace '((("/my-root/newfile" "/my-root/project.md")))))))


