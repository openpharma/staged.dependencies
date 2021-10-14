# staged.dependencies 0.2.3

## Breaking changes

* `install_deps_app` now takes projects from remote rather than local directory, which allows different `ref` to be used. See example for more details. It also does not allow `local_repos` argument.

## New feature and improvements

* `install_deps_app` allows `repo`, `host` and `ref` to be configured by users.

## Other improvements

* Added a `NEWS.md` file to track changes to the package.

## Bugfixes

* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed.
*More gracefully handles error in `install_deps_app`. 
