# staged.dependencies 0.2.3

## Breaking changes

## New feature and improvements

* `get_all_external_dependencies` now returns the vector of external packages ordered by install order. 

## Other improvements

* Added a `NEWS.md` file to track changes to the package.

## Bugfixes

* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed. 
