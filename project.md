# Project Management

## Backlog
- ignore hidden directories for most recent file display in open project
- C-c C-t binding for lisp testing (all)
  - via hook or via projectile? => both work, hook is fine at the time
- C-c C-c t for test at point
- hook to open magit always in 2nd window
- add proj-run-interpreter-on-side to open an interpreter
- projectile cmd to do a grep with dired buffer
- Refactor mock-files so that it stays in test scope
- separate tools from proj code

## Done
+ refactor el files
  - import projectile stuff in proj, move proj to src
  - create an elisp project type with all the good bindings
  - remove lisp hooks
  - appropriate test/impl shortcut via projectile
+ strict mapping test/impl
+ refactor functions for them to be namespace compliant
+ separate tools/proj code
+ Git the project
+ use second latest file to always display 2 files ####
  + change get-rel-file test to account for this
  + update get-rel-file so it does the job
+ Shortcut for tdd
  + have a shortcut to save, eval and test at point just like elpy
  + have a shortcut to run all tests



## Larger Backlog
- emacs should autosave correctly
  - done as a minor mode?
  - version history?
	- conflicts with git
