# Development Stages via git branch naming convention

## Usage

Set the access tokens to download from the git hubs (GitHub, Gitlab) with read privilege for the repositories in question.

```
Sys.setenv("GITHUB_PAT" = "<token>")
Sys.setenv("ROCHE_GITHUB_PAT" = "<token>")
Sys.setenv("ROCHE_GITLAB_PAT" = "<token>")
```

You can have these tokens set permanently by putting the information into the `~/.Renviron` file, e.g. by calling `usethis::edit_r_environ()`:

```
GITHUB_PAT=<token>
ROCHE_GITHUB_PAT=<token>
ROCHE_GITLAB_PAT=<token>
```

Roche specific: Put this into your `~/.Rprofile` (by calling `usethis::edit_r_profile()`):
```
options(
  staged.dependencies.token_mapping = c(
    "https://github.com" = "GITHUB_PAT",
    "https://gitlab.com" = "GITLAB_PAT",
    "https://github.roche.com" = "ROCHE_GITHUB_PAT",
    "https://code.roche.com" = "ROCHE_GITLAB_PAT"
  )
)
```
You can also call this later once the package is loaded.

Load the addins with `library(staged.dependencies)`, then search for them in the addins menu.
Alternatively, you can run them explicitly and change the default arguments:

```{r}
# latex may not be installed, so `R CMD check` does not fail because of it
Sys.setenv(RCMDCHECK_ARGS = "--no-manual")

check_downstream(
  project = "../stageddeps.electricity"
)

install_upstream_deps(project = "../stageddeps.electricity", verbose = 1)

check_downstream(
  project = "../stageddeps.electricity", 
  downstream_repos = list(list(repo = "maximilian_oliver.mordig/stageddeps.house", host = "https://code.roche.com"))
)

install_upstream_deps(project = "../stageddeps.house", feature = "fix1@feature1@master", verbose = 1)
```

Remember to restart the R session after installing packages that were already loaded.

## Example Setup

This package can be tested on the following setup of packages.
![Dependencies. Arrow point from upstream to downstream packages](StagedDepsExample.png)

See https://code.roche.com/maximilian_oliver.mordig/stageddeps.water and the related packages.

You can check that the right branches of the packages are installed with:

```{r}
stageddeps.house::get_house()
stageddeps.water::get_water()
stageddeps.garden::get_garden()
stageddeps.food::get_food()
stageddeps.elecinfra::get_elecinfra()
stageddeps.electricity::get_electricity()
```

## Workflow

Suppose one implements a new feature called `feature1` that involves `repoB, repoC` with the dependency graph `repoA --> repoB --> repoC`, where `repoA` is an additional upstream dependency. One then notices that `feature1` requires a fix in `repoB`, so one creates a new branch `fix1@feature1@devel` on `repoB`. The setup can be summarized as follows:
```
repoB: fix1@feature1@devel, feature1@devel, devel
repoC: feature1@devel, devel
repoA: devel
```
A PR on repoB `fix1@feature1@devel --> feature1@devel` takes `repoB:fix1@feature1@devel, repoC: feature1@devel, repoA: devel`. This can be checked by setting `feature = fix1@feature1@devel` and running `R CMD check` on `repoC`, or `check_downstream` on `repoB` (adding `repoC` to its downstream dependencies).
The PR on `repoB` and  `repoC` `feature1@devel --> devel` takes `repoB:feature1@devel, repoC: feature1@devel, repoA: devel`, setting `feature = feature1@devel`.

## Addins
When you are working from a project todo, includes gitignored files
caches local repos with sha
use hashing

## Troubleshooting

`git2r::clone` may fail. Check that the git host is reachable (VPN may be needed) and that the access token has read privileges for the repositories.
