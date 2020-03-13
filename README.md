# Proj: A few project utilities for emacs

Mostly consists of an proj-open command and emacs lisp project test/run functions and an elisp project type for projectile. Can be used as-is or integrated to projectile project functions (as done in ``projectile-custom.el``).

## Functions
``M-x proj-open / proj-open-from-name`` opens recent files for a project, see open.el for more details.

``M-x proj--lisp-load-and-test-all`` for lisp projects, loads all lisp files and runs ert.

## Bindings
As set in ``projectile-custom.el``, C-c C-t used to test project, C-c C-r to run it, and M-p s to grep in it, as well as C-c C-z to open scratch (and thus eval lisp forms) on the left window.

## Projectile elisp project type
Emacs lisp projects are deteted by the presence of a ``run.el`` file which should run the project when loaded. ``projectile-run-project`` will run ``run.el``.

A test command to load all el files (incl. tests) and run ert is defined and bound to ``projectile-test-project``.

