(load "src/open")
(load "test/mocks")

;;; proj--get-relevant-files

(ert-deftest proj--get-relevant-files ()

  ;; if there are files, return the most recent first and the project file then
  (let ((mock-files (list mock-oldfile mock-newfile mock-projfile)))
    (should (equal (proj--get-relevant-files mock-files) '("newfile" "project.md"))))

  ;; README.md is a valid project file
  (let ((mock-files (list mock-oldfile mock-newfile mock-readmefile)))
    (should (equal (proj--get-relevant-files mock-files) '("newfile" "README.md"))))

  ;; If a project file is most recent, return it first, and the second most recent then
  (let ((mock-files (list mock-olderfile mock-readmefile)))
    (should (equal (proj--get-relevant-files mock-files) '("README.md" "olderfile"))))
  (let ((mock-files (list mock-oldfile mock-projfile mock-olderfile)))
    (should (equal (proj--get-relevant-files mock-files) '("project.md" "oldfile"))))

  ;; If there are no project file, return only the latest
  (let ((mock-files (list mock-oldfile mock-newfile mock-olderfile)))
    (should (equal (proj--get-relevant-files mock-files) '("newfile" "oldfile")))))

(ert-deftest valid-emacs-regexp ()
  ;;; checks the regexp for emacs temp files work
  (let ((test-list '("/file1.md" "/home/file2" "file3~" "/#file4" "/truc/#file6.el#" ".file7")))
    (should (equal
	     (seq-filter (lambda (s) (string-match-p remove-unwanted-files-regexp s)) test-list)
	     '("/file1.md" "/home/file2" "/#file4")))))
