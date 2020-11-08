;;; proj-clj -- Clojure-specific project features, partly built on cider

;; -*- lexical-binding: t -*-
;; lexical binding required for proj--refresh-ns-before
;;; Commentary:

;;; Code:
(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-auto-jump-to-error nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-save-file-on-load t)
(setq cider-save-files-on-cider-ns-refresh t)

(defun autoeval-clojure-buffers ()
  "Set up autoevaluation of clojure buffers.

Clojure buffers with a connected REPL are evaluated automatically
when they are saved, or when a cider test command is run while
they are visited (in which case they are saved before eval)."
  (add-hook 'pre-command-hook
	    (lambda ()
	      (when (and (symbolp this-command)
			 (string-prefix-p "cider-test" (symbol-name this-command)))
		(if (buffer-modified-p)
		    (save-buffer)
		  (proj-clj-reload-current-buffer))))))

(defun proj--clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (enable-paredit-mode))

(add-hook 'clojure-mode-hook #'proj--clojure-mode-hook)

(defun proj--refresh-ns-before (orig-fn &rest orig-args)
  "Function to advise a command to refresh the project evaluation
before executing. Using handle-response to catch when the refresh
is done, since ns-refresh is async."
  (define-advice cider-ns-refresh--handle-response
      (:after (resp &rest rst) post-refresh)
    (when (member "ok" (nrepl-dict-get resp "status"))
      (apply orig-fn orig-args)
      (advice-remove 'cider-ns-refresh--handle-response
		   #'cider-ns-refresh--handle-response@post-refresh)))
  (cider-ns-refresh))

;; All commands of the list below will be advised to refresh before running
(let ((commands-to-advise '(cider-test-run-ns-tests
			    cider-test-run-test
			    cider-test-run-project-tests
			    cider-test-run-loaded-tests
			    cider-eval-last-sexp
			    cider-eval-last-sexp-to-repl
			    cider-eval-defun-at-point)))
  (mapc
   (lambda (symb)
     (advice-add symb :around #'proj--refresh-ns-before))
   commands-to-advise))
