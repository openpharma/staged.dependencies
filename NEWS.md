# staged.dependencies 0.2.3

## Breaking changes

## New feature and improvements

## Other improvements

* Added a `NEWS.md` file to track changes to the package.
* Fallback branch for repositories is now the default branch of the repo, not hard-coded as `main`.

## Bugfixes

* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed. 
