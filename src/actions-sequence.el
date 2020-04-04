;;; Sequence of actions to perform when opening a project Each action
;;; is a list of size 3 (tags function-symbol args). The project
;;; must have all tags for :function-sybol to be run. Existing tags are
;;;
;;; - :first-opened if the project has not been opened before in this session
;;; - :already-opened otherwise
;;; - :lein-test if the projectile type of the project is lein-test
;;; - :{project-type} similarly for any project type
;;; - :git / :no-git depending on the existence of a .git dir at the root
;;;
;;; If :tags is nil then the function is run :args are optional args
;;; to the function. Any keyword in args is considered an "action var"
;;; and replaced by the corresponding value in the proj--state
;;; :actions-var plist. Available action vars are:
;;;
;;; :most-recent-file
;;; :project-file
;;; :2nd-most-relevant -> see proj--get-relevant-file
;;;
;;; If an action var does not exist, the program fails. However, if an
;;; action var exists and resolves to nil, then it is properly
;;; replaced

(setq
 proj--actions-seq
 (list
  '(nil delete-other-windows nil)
  '(nil proj--add-to-path nil)
  '((:already-opened) find-file (:most-recent-file))
  '((:already-opened) find-file-other-window (:2nd-most-relevant))
  '((:first-opened) proj-open-project-file (:project-file))
  '((:git :first-opened) magit-status nil)
  '((:first-opened :lein-test) cider-jack-in (nil))
  '(nil other-window (1))))
