;;; Sequence of actions to perform when opening a project Each action
;;; is a list of size 3 (tags function-symbol args). The project
;;; must have all tags for :function-sybol to be run. Existing tags are
;;;
;;; - :first-open if the project has not been opened before in this session
;;; - :already-opened otherwise
;;; - :lein-test if the projectile type of the project is lein-test
;;;
;;; If :tags is nil then the function is run :args are optional args
;;; to the function. Any keyword in args is replaced by the
;;; corresponding value in the proj--state :actions-var plist.

(setq proj--actions-seq
      (list
       '(nil delete-other-windows nil)))
