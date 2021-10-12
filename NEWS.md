# staged.dependencies 0.2.3

## Breaking changes

## New feature and improvements

* `staged.dependencies` can handle only having access to part of the internal dependency graph.

## Other improvements

* Added a `NEWS.md` file to track changes to the package.

## Bugfixes

* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed. 
