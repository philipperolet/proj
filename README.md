# Proj: A few project utilities for emacs

Commands to open projects in a nice way, emacs lisp project test/run
functions, and an elisp project type for projectile. Meant for
integration to projectile project functions (as done in
``projectile-custom.el``).

## Functions
`proj-open` opens projects executing various actions depending on
projectile project type with multiple ways of opening are provided
through proj-open-xxx functions given in argument

`proj-open-relevant` opens recent files for a project,
`proj-open-pfile` and `proj-open-project-file` open the project file
using projectile to find the root

See open.el for more details.

``M-x proj--lisp-load-and-test-all`` for lisp projects, loads all lisp files and runs ert.

## Bindings
Shortcuts are set in ``projectile-custom.el`` to:
- test project
- run it, 
- grep in it, 
- open scratch (and thus eval lisp forms) on the left window

## Projectile elisp project type
Emacs lisp projects are deteted by the presence of a ``run.el`` file which should run the project when loaded. ``projectile-run-project`` will run ``run.el``.

A test command to load all el files (incl. tests) and run ert is defined and bound to ``projectile-test-project``.

