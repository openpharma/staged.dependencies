# staged.dependecies 0.3.0

* Support subdirectories in configuration.
* Remove `renv` files and clean the project in that regard.

# staged.dependecies 0.2.8

* Updated code for changes in `tidyselect 1.2.0`.
* Fixed issue with storage directory being overwritten if set by user.
* Updated copying of config file so it doesn't fail if file already exists.

# staged.dependencies 0.2.7

* Fixed a bug whereby the cache file for packages with compiled code was not reset after installation thereby `dependency_table` failed on a 2nd run.

# staged.dependencies 0.2.6

* Fixed bug whereby `renv` did not recognize internal dependencies already installed.

# staged.dependencies 0.2.5

* Added `renv_files` element into `dependency_structure` object to capture (in JSON) the renv.lock files from internal dependencies for future processing. The `renv_profile` argument to `dependency_table` allows you to choose which renv profile to include if not using the default one.
* Use `renv::install` to install external dependencies if inside an `renv` environment so that the renv cache is used

# staged.dependencies 0.2.4

## Bugfixes
* Fixed error where package actions (i.e. `check`, `build`) were skipped.

# staged.dependencies 0.2.3

## Breaking changes

* Soft deprecated `direction = c("upstream", "downstream")`, now use `direction = "all"`.
* `dry_install` and `dry_install_and_check` arguments have both been renamed `dry`.
* `dependency_packages`, `downstream_packages` and `packages_to_process` arguments have all been renamed `package_list`.
* `local_repos` argument to `dependency_table` defaults to `NULL` if `project_type` is `"repo@host"`.
* `install_deps_app` now takes projects from remote rather than local directory, which allows different `ref` to be used. See example for more details. It also does not allow `local_repos` argument.
* Removed upgrade staged.dependencies package addin.

## New feature and improvements

* `staged.dependencies` can handle only having access to part of the internal dependency graph.
* `install_deps_app` allows `repo`, `host` and `ref` to be configured by users.
* Created addin to install current project's dependencies alongside current project.
* `get_all_external_dependencies` now returns the vector of external packages ordered by install order and the core R packages are not included.

## Other improvements

* Added a `NEWS.md` file to track changes to the package.
* `upgrade` argument of `remotes::install_deps` to choose whether to install external dependencies is now exposed (and default is `never`). For example, setting `upgrade` to `always` will update all external dependencies giving an up-to-date environment.
* Set default R CMD check args to be `--no-multiarch --with-keep.source --install-tests`.

## Bugfixes

* Allow a remote other than `origin`.
* `install_external_deps` argument to `install_deps_app` is now respected.
* Fix bug when calling `check_downstream` after `install_deps` whereby incorrect error was shown saying sha has changed.
* Fallback branch for repositories can now be specified and is not hard-coded as `main`.
* `check_downstream` will not stop on first package error but will run on all expected packages and all failures are output.
* More gracefully handles error in `install_deps_app`.
