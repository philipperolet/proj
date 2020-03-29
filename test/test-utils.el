(load "src/utils")
(load "test/mocks")

(ert-deftest proj--argmax--integer-lists ()
  ;; integer-comparison is like max but handling nil values
  (let ((integer-comparison (lambda (a b) (if (not a) b (if (not b) a (max a b))))))

    (should (equal (proj--argmax '(1 2 12 3) integer-comparison)
		   12))
  
    (should (equal (proj--argmax '(nil 0 1 1) integer-comparison)
		   1))
  
    (should (equal (proj--argmax '(-4) integer-comparison)
		   -4))))

(ert-deftest proj--argmax--with-lists-of-lists ()
  ;; compare-3rd-elt assumes the comparing elt is the 3rd one
  (let ((compare-3rd-elt (lambda (a b)
			   (cond ((not a) b)
				 ((not b) a)
				 ((> (nth 2 a) (nth 2 b)) a)
				 (t b)))))
    
    (should (equal (proj--argmax '(("elt1" val1 3) ("elt2" val2 8) ("elt3" val3 -2))
			   compare-3rd-elt)
		   '("elt2" val2 8)))))


(ert-deftest proj--compare-files-modif-date-- ()
  (should (equal (proj--compare-files-by-modif-date mock-newfile mock-projfile) mock-newfile)))

(ert-deftest proj--get-dir-list-from-paths-- ()
  (cl-letf (((symbol-function 'directory-files-and-attributes) #'mock-dirfilesandattr))
    (should (equal (proj--get-dir-list-from-paths '("/project/path/path1" "/project/path/path2"))
		   (list '("dir1" "/project/path/path1")
			 '("dir2" "/project/path/path1")
			 '("dir3" "/project/path/path2"))))))

(ert-deftest proj--dir-files-and-attrs-recursive--base ()
 
  ;; Mock of directory-files-recursively / file-attributes
  (cl-letf (((symbol-function 'directory-files-recursively) (lambda (p r) '("file1" "file2")))
	    ((symbol-function 'file-attributes) (lambda (f) '("attr1" 2 3))))
  ;; Checks that it returns appropriate (filename file-attrs) pairs    
    (should (equal (proj--dir-files-and-attrs-recursive nil nil)
		   '(("file1" "attr1" 2 3) ("file2" "attr1" 2 3))))))

(ert-deftest proj--dir-files-and-attrs-recursive--ignore-hidden-dir-files ()
  (cl-letf (((symbol-function 'directory-files-recursively)
	     (lambda (p r) '("/path/file1.el"
			     "/.hidden/file2"
			     "/not.hidden/file3"
			     "/multiple/.hidden/file4.el"
			     ".starting.hidden/file5"
			     ".singlehiddenfile")))
	    ((symbol-function 'file-attributes) (lambda (f) '("attr1" 2 3))))
    (should (equal (proj--dir-files-and-attrs-recursive nil nil)
		   '(("/path/file1.el" "attr1" 2 3) ("/not.hidden/file3" "attr1" 2 3))))))

(ert-deftest proj--get--test ()
  (let ((proj--state '(:p1 32 :p2 (:p21 14 :p22 "hello"))))
    (should (equal (proj--get :p1) 32))
    (should (equal (proj--get :p2 :p21) 14))
    (should (equal (proj--get :p2 :p22) "hello"))))

(ert-deftest proj--set--test ()
  (let ((proj--state '(:p1 32 :p2 (:p21 14 :p22 "hello"))))
    (proj--set :p1 12)
    (proj--set :p2 :p23 "hi")
    (proj--set :p2 :p22 "gutentag")
    (should (equal (proj--get :p1) 12))
    (should (equal (proj--get :p2 :p21) 14))
    (should (equal (proj--get :p2 :p22) "gutentag"))
    (should (equal (proj--get :p2 :p23) "hi"))))
