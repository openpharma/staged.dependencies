# staged.dependencies 0.2.3

## Breaking changes

* Dependency shiny app now takes projects from remote rather than local directory, which allows different `ref` to be used. See example for more details.

## New feature and improvements

## Other improvements

* Added a `NEWS.md` file to track changes to the package.

## Bugfixes

* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed. 
