# Project Management

## Backlog

### Icebox
- hook to open magit always in 2nd window
- Refactor mock-files so that it stays in test scope
- C-c C-c t for test at point

## Done
+ add proj-run-interpreter-on-side to open an interpreter
+ fix project file opening bug
+ projectile cmd to do a grep with dired buffer
+ ignore hidden directories for most recent file display in open project
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
