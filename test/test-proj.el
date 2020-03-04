;;;; Tests for proj

(load "src/open-project")
(load "test/test-proj-mocks")

;;; Argmax

(ert-deftest test-argmax-integer-lists ()
  ;; integer-comparison is like max but handling nil values
  (let ((integer-comparison (lambda (a b) (if (not a) b (if (not b) a (max a b))))))

    (should (equal (argmax '(1 2 12 3) integer-comparison)
		   12))
  
    (should (equal (argmax '(nil 0 1 1) integer-comparison)
		   1))
  
    (should (equal (argmax '(-4) integer-comparison)
		   -4))))

(ert-deftest argmax-with-lists-of-lists ()
  ;; compare-3rd-elt assumes the comparing elt is the 3rd one
  (let ((compare-3rd-elt (lambda (a b)
			   (cond ((not a) b)
				 ((not b) a)
				 ((> (nth 2 a) (nth 2 b)) a)
				 (t b)))))
    
    (should (equal (argmax '(("elt1" val1 3) ("elt2" val2 8) ("elt3" val3 -2))
			   compare-3rd-elt)
		   '("elt2" val2 8)))))

;;; get-relevant-files

(ert-deftest get-relevant-files ()
  ;; if no file in path or only directories, returns only the current dir
  (let ((only-directories '(("dir1" (t 1)) ("dir2" (t 2))))
	(empty-list ()))
    (should (equal (get-relevant-files empty-list) (list ".")))
    (should (equal (get-relevant-files only-directories) (list "."))))

  ;; if there are files, return the most recent first and the project file then
  (let ((mock-files (list mock-oldfile mock-newfile mock-projfile)))
    (should (equal (get-relevant-files mock-files) '("newfile" "project.md"))))

  ;; README.md is a valid project file
  (let ((mock-files (list mock-oldfile mock-newfile mock-readmefile)))
    (should (equal (get-relevant-files mock-files) '("newfile" "README.md"))))

  ;; If a project file is most recent, return it first, and the second most recent then
  (let ((mock-files (list mock-olderfile mock-readmefile)))
    (should (equal (get-relevant-files mock-files) '("README.md" "olderfile"))))
  (let ((mock-files (list mock-oldfile mock-projfile mock-olderfile)))
    (should (equal (get-relevant-files mock-files) '("project.md" "oldfile"))))

  ;; If there are no project file, return only the latest
  (let ((mock-files (list mock-oldfile mock-newfile mock-olderfile)))
    (should (equal (get-relevant-files mock-files) '("newfile" "oldfile"))))
  
  ;; Ignore emacs temp files
  (let ((mock-files (list mock-oldfile mock-newfile-tilde mock-newfile-hash mock-olderfile)))
    (should (equal (get-relevant-files mock-files) '("oldfile" "olderfile")))))


(ert-deftest compare-files-modif-date ()
  (should (equal (compare-files-by-modif-date mock-newfile mock-projfile) mock-newfile)))

(ert-deftest get-first-matching-file ()
  (should (equal (get-first-common-element '("a" "b" "c") '("d" "e" "b" "c" "f")) "b")))

(ert-deftest test-get-dir-list-from-paths ()
  (cl-letf (((symbol-function 'directory-files-and-attributes) #'mock-dirfilesandattr))
    (should (equal (get-dir-list-from-paths '("path1" "path2"))
		   (list '("dir1" "path1") '("dir2" "path1") '("dir3" "path2"))))))
