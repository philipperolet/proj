(setq mock-oldfile '("/project/path/oldfile" nil 1 "lh" "users"
		     (20000 23 0 0)
		     (20500 64019 50040 152000)
		     (20614 64555 902289 872000)
		     122295 "-rw-rw-rw-"
		     t (5888 2 . 43978)
		     (15479 . 46724))
      mock-newfile '("/project/path/newfile" nil 1 "lh" "users"
		     (20000 23 0 0)
		     (20700 64019 50040 152000)
		     (20614 64555 902289 872000)
		     122295 "-rw-rw-rw-"
		     t (5888 2 . 43978)
		     (15479 . 46724))
      mock-newerfile '("/project/path/newerfile" nil 1 "lh" "users"
		       (20000 23 0 0)
		       (20800 64019 50040 152000)
		       (20614 64555 902289 872000)
		       122295 "-rw-rw-rw-"
		       t (5888 2 . 43978)
		       (15479 . 46724))
      mock-projfile '("/project/path/project.md" nil 1 "lh" "users"
		      (20000 23 0 0)
		      (20614 64019 50040 152000)
		      (20614 64555 902289 872000)
		      122295 "-rw-rw-rw-"
		      t (5888 2 . 43978)
		      (15479 . 46724))
      mock-readmefile '("/project/path/README.md" nil 1 "lh" "users"
			(20000 23 0 0)
			(20613 64019 50040 152000)
			(20614 64555 902289 872000)
			122295 "-rw-rw-rw-"
			t (5888 2 . 43978)
			(15479 . 46724))
      mock-olderfile '("/project/path/olderfile" nil 1 "lh" "users"
		       (20000 23 0 0)
		       (20400 64019 50040 152000)
		       (20614 64555 902289 872000)
		       122295 "-rw-rw-rw-"
		       t (5888 2 . 43978)
		       (15479 . 46724))
      mock-dir1 '("dir1" t 1 "lh" "users"
		  (20000 23 0 0)
		  (20400 64019 50040 152000)
		  (20614 64555 902289 872000)
		  122295 "-rw-rw-rw-"
		  t (5888 2 . 43978)
		  (15479 . 46724))
      mock-dir2 '("dir2" t 1 "lh" "users"
		  (20000 23 0 0)
		  (20400 64019 50040 152000)
		  (20614 64555 902289 872000)
		  122295 "-rw-rw-rw-"
		  t (5888 2 . 43978)
		  (15479 . 46724))
      mock-dir3 '("dir3" t 1 "lh" "users"
		  (20000 23 0 0)
		  (20400 64019 50040 152000)
		  (20614 64555 902289 872000)
		  122295 "-rw-rw-rw-"
		  t (5888 2 . 43978)
		  (15479 . 46724)))

(defun mock-dirfilesandattr (x)
  (if (equal x "/project/path/path1")
      (list mock-oldfile mock-newfile mock-dir1 mock-dir2)
    (list mock-newfile mock-dir3)))
