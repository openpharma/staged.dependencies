# staged.dependencies 0.2.3

## Breaking changes

## New feature and improvements

* `get_all_external_dependencies` now returns the vector of external packages ordered by install order and the core R packages are not included. 

## Other improvements

* Added a `NEWS.md` file to track changes to the package.
* Fallback branch for repositories can now be specified and is not hard-coded as `main`.
* `check_downstream` will not stop on first package error but will run on all expected packages and all failures are output. 


## Bugfixes

* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed. 
