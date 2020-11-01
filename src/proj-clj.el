;; Clojure-specific project features, partly built on cider

(setq cider-repl-display-help-banner nil)
(setq cider-repl-pop-to-buffer-on-connect nil)

(defun proj-clj-reload-current-buffer ()
  (when (and (string-match "\\.clj[cs]?$" (buffer-name))
	     (cider-repls))
    (cider-load-buffer)))

(defun autoeval-clojure-buffers ()
  "Set up autoevaluation of clojure buffers.

Clojure buffers with a connected REPL are evaluated automatically
when they are saved, or when a cider test command is run while
they are visited (in which case they are saved before eval)."
  (add-hook 'after-save-hook #'proj-clj-reload-current-buffer)
  (add-hook 'pre-command-hook
	    (lambda ()
	      (when (and (symbolp this-command)
			 (string-prefix-p "cider-test" (symbol-name this-command)))
		(message "Hooked on %s" (symbol-name this-command))
		(if (buffer-modified-p)
		    (save-buffer)
		  (proj-clj-reload-current-buffer))))))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (enable-paredit-mode))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(autoeval-clojure-buffers)
