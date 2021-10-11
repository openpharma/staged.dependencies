# staged.dependencies 0.2.3

## Breaking changes

* Soft deprecated `direction = c("upstream", "downstream")`, now use `direction = "all"`.

## New feature and improvements

## Other improvements

* Added a `NEWS.md` file to track changes to the package.
* `upgrade` argument of `remotes::install_deps` to choose whether to install external dependencies is now exposed (and default is `never`). For example, setting `upgrade` to `always` will update all external dependencies giving an up-to-date environment.
* Set default R CMD check args to be `--no-multiarch --with-keep.source --install-tests`.

## Bugfixes

* `install_external_deps` argument to `install_deps_app` is now respected.
