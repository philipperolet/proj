# Project Management

#### Bug when switching to danone-adf
funcall: Variable binding depth exceeds max-specpdl-size

#### Autojackin lors du lancement de projets clojure
#### Auto run tests après autojackin

## Backlog
- command when running / testing to reset all previous definitions
- projectile search uses dired buffer

### Icebox
- M-. works on specs
- doc available on specs
- specify interface for code

- open magit always in 2nd window

## Done
#### Refactor for testing: push projectile dependency out
#### use proj--project-files var in proj-open-project-file
instead of direct "project.md"

#### proj-open: use pfile for first-time opening, relevant files for next
#### create proj-open responsible for deciding how/when to call proj-relevant/proj-pfile
#### ouvrir project.md directement dans la fenêtre principale
que ce soit via open-project ou via C-x p
avec magit ou les tests en second

- brancher projectile lein à cider pour la commande de test**
- change command for projectile test to c-c c-t p
- autojackin when opening clj files in a leiningen project
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
