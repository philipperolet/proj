;; List of packages I use and function to auto-install them

;; install ivy first, otherwise emacs crashes
(setq package-list '(cider magit projectile flycheck-clj-kondo markdown-mode js2-mode paredit clj-refactor))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, install if
it’s not. Return a list of installed packages or nil for every
skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (package-install package)))
   packages))

(defun ensure-packages-installed ()
  (interactive)
  ;; make sure to have downloaded archive description.
  ;; Or use package-archive-contents as suggested by Nicolas Dudebout
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))
  (apply 'ensure-package-installed package-list))

