# this file does not require internet access since it mocks git remote operations


local_pkgs <- c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food", "stageddeps.house", "stageddeps.garden", "stageddeps.water")

# assumes that repo stageddeps.food is local
# copies repos from TESTS_GIT_REPOS to appropriate locations in cache_dir
mock_rec_checkout_internal_deps <- function(source_dir) {
  function(repos_to_process, ...) {
    cat(paste0("Mocking rec_checkout_internal_deps", "\n"))
    expect_equal(repos_to_process, list(list(repo = "openpharma/stageddeps.food", host = "https://github.com")))

    # stageddeps.food is local
    internal_deps <- data.frame(
      pkg = local_pkgs,
      repo = paste0("openpharma/", local_pkgs),
      host = rep("https://github.com", 6),
      ref = c("main", "main", "local (main)", "main", "main", "main"),
      sha = rep("test", 6),
      accessible = rep(TRUE, 6),
      installable = rep(TRUE, 6),
      stringsAsFactors = FALSE
    ) %>% dplyr::mutate(cache_dir = unlist(Map(get_repo_cache_dir, repo, host, local = grepl("^local ", ref))))
    # fs::dir_copy does not seem to be vectorized (although stated in the doc) -> use Map
    clear_cache()
    Map(fs::dir_copy, file.path(source_dir, internal_deps$pkg), internal_deps$cache_dir)
    return(internal_deps %>% dplyr::select("repo", "host", "cache_dir", "ref", "sha", "accessible", "installable"))
  }
}

test_that("dependency_table works", {
  mockery::stub(dependency_table, "rec_checkout_internal_deps", mock_rec_checkout_internal_deps(TESTS_GIT_REPOS))

  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")
  git2r::remote_set_url(repo_dir, name = "origin", url = "https://github.com/openpharma/stageddeps.food.git")

  with_tmp_cachedir({
    expect_output(
      dep_table <- dependency_table(repo_dir, ref = "main"),
      regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
    )

    # check output of `dependency_table` matches ground-truth
    expect_s3_class(dep_table, "dependency_structure")
    expect_equal(dep_table$project_type, "local")
    expect_equal(dep_table$current_pkg, "stageddeps.food")
    expect_equal(
      dep_table$table,
      data.frame(
        package_name = c(
          "stageddeps.food", "stageddeps.elecinfra",
          "stageddeps.electricity", "stageddeps.house", "stageddeps.garden",
          "stageddeps.water"
        ),
        type = factor(c("current", "upstream", "upstream", "downstream", "other", "other"),
          levels = c("current", "upstream", "downstream", "other")
        ),
        distance = c(0, 1, 2, 1, NA, NA),
        ref = c("local (main)", "main", "main", "main", "main", "main"),
        install_index = c(3, 1, 2, 5, 6, 4),
        stringsAsFactors = FALSE
      ) %>% dplyr::mutate(
        repo = paste0("openpharma/", package_name),
        host = rep("https://github.com", 6),
        cache_dir = unlist(Map(get_repo_cache_dir, repo, host, local = grepl("^local ", ref))),
        sha = rep("test", 6),
        accessible = rep(TRUE, 6),
        installable = rep(TRUE, 6)
      ) %>%
        dplyr::select(
          "package_name", "type", "distance", "ref", "repo", "host",
          "sha", "cache_dir", "accessible", "installable", "install_index"
        )
    )

    expect_output(
      dep_table2 <- dependency_table(repo_dir, ref = "main", direction = "upstream"),
      regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
    )
    # check direction upstream only, should not matter since yamls agree with DESCRIPTION files
    expect_equal(dep_table[names(dep_table) != "direction"], dep_table2[names(dep_table2) != "direction"])
  })
})

test_that("dependency_table wih local_pkgs works", {
  copied_ecosystem <- tempfile("copied_ecosystem")
  fs::dir_copy(TESTS_GIT_REPOS, copied_ecosystem)
  repo_dir <- file.path(copied_ecosystem, "stageddeps.house")

  lapply(local_pkgs, function(local_pkg) {
    repo_dir <- file.path(copied_ecosystem, local_pkg)
    git2r::checkout(repo_dir, "main")
    git2r::remote_set_url(repo_dir,
      name = "origin",
      url = paste0("https://github.com/openpharma/", local_pkg, ".git")
    )
    # set config (needed for automation)
    git2r::config(git2r::repository(repo_dir), user.name = "github.action", user.email = "gh@action.com")
  })


  local_pkgs <- c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food", "stageddeps.house", "stageddeps.garden", "stageddeps.water")
  local_repos <- data.frame(
    repo = paste0("openpharma/", local_pkgs),
    host = rep("https://github.com", 6),
    directory = file.path(copied_ecosystem, local_pkgs),
    stringsAsFactors = FALSE
  )

  # stageddeps.garden has branch fixgarden@main, so it should check it out
  expect_error(
    dependency_table(repo_dir, ref = "fixgarden@main", local_repos = local_repos),
    regexp = "must check out branch fixgarden@main", fixed = TRUE
  )

  git2r::checkout(file.path(copied_ecosystem, "stageddeps.garden"), branch = "fixgarden@main")
  expect_silent(
    dependency_table(repo_dir, ref = "fixgarden@main", local_repos = local_repos, verbose = 0)
  )
})

test_that("check_yamls_consistent works", {
  # copy to new directory, so we can modify branch which is then copied to cache_dir in rec_checkout_repos
  copied_ecosystem <- tempfile("copied_ecosystem")
  fs::dir_copy(TESTS_GIT_REPOS, copied_ecosystem)

  mockery::stub(dependency_table, "rec_checkout_internal_deps", mock_rec_checkout_internal_deps(copied_ecosystem))

  with_tmp_cachedir({
    # missing staged_dependencies.yaml in stageddeps.garden
    repo_dir <- file.path(copied_ecosystem, "stageddeps.food")
    git2r::checkout(repo_dir, "main")
    git2r::remote_set_url(repo_dir, name = "origin", url = "https://github.com/openpharma/stageddeps.food.git")
    expect_output(
      expect_error(
        check_yamls_consistent(
          dependency_table(repo_dir, ref = "main")
        ),
        regexp = "for package stageddeps.garden", fixed = TRUE
      ),
      regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
    )

    git2r::checkout(file.path(copied_ecosystem, "stageddeps.garden"), branch = "fixgarden@main")
    expect_output(
      check_yamls_consistent(
        dependency_table(repo_dir, ref = "fixgarden@main")
      ),
      regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
    )
  })
})

test_that("plot.dependency_structure works", {
  mockery::stub(dependency_table, "rec_checkout_internal_deps", mock_rec_checkout_internal_deps(TESTS_GIT_REPOS))
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::remote_set_url(repo_dir, name = "origin", url = "https://github.com/openpharma/stageddeps.food.git")
  git2r::checkout(repo_dir, "main")

  # check that it works by saving the plot to a file which requires the plot code to be
  # executed (otherwise lazy eval)
  expect_output(
    dep_table <- dependency_table(repo_dir, ref = "main"),
    regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
  )
  plot_file <- tempfile("dep_plot", fileext = ".html")
  plot.dependency_structure(dep_table) %>% visNetwork::visSave(plot_file)
  expect_true(file.exists(plot_file))
  expect_true(file.info(plot_file)$size > 0) # expect non-empty
})

test_that("install_deps works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")
  git2r::remote_set_url(repo_dir, name = "origin", url = "https://github.com/openpharma/stageddeps.food.git")
  mockery::stub(dependency_table, "rec_checkout_internal_deps", mock_rec_checkout_internal_deps(TESTS_GIT_REPOS))
  capture.output(dep_table <- dependency_table(repo_dir, ref = "fixgarden@main")) # capture.output to make silent

  mockery::stub(install_deps, "run_package_actions", function(pkg_actions, ...) {
    pkg_actions
  })

  # check install_direction = "upstream"
  expected_result <- data.frame(
    package_name = c(
      "stageddeps.elecinfra", "stageddeps.electricity",
      "stageddeps.food"
    ),
    stringsAsFactors = FALSE
  )
  expected_result$actions <- rep(list("install"), 3)
  expect_equal(
    install_deps(dep_table, dry_install = TRUE, install_direction = "upstream")[, c("package_name", "actions")],
    expected_result
  )

  # check install_direction = "downstream"
  # in theory may fail because topological order is not unique,
  # although topological order is not unique our implementation should be deterministic
  expected_result <- data.frame(
    package_name = c(
      "stageddeps.elecinfra", "stageddeps.electricity",
      "stageddeps.food", "stageddeps.water", "stageddeps.house"
    ),
    stringsAsFactors = FALSE
  )
  expected_result$actions <- rep(list("install"), 5)

  expect_equal(
    install_deps(dep_table, dry_install = TRUE, install_direction = "downstream")[, c("package_name", "actions")],
    expected_result
  )

  # check install_direction = "all"
  # in theory may fail because topological order is not unique,
  # although topological order is not unique our implementation should be deterministic
  expected_result <- data.frame(
    package_name = c(
      "stageddeps.elecinfra", "stageddeps.electricity",
      "stageddeps.food", "stageddeps.water", "stageddeps.house",
      "stageddeps.garden"
    ),
    stringsAsFactors = FALSE
  )
  expected_result$actions <- rep(list("install"), 6)

  expect_equal(
    install_deps(dep_table, dry_install = TRUE, install_direction = "all")[, c("package_name", "actions")],
    expected_result
  )
})

test_that("check_downstream works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")
  git2r::remote_set_url(repo_dir, name = "origin", url = "https://github.com/openpharma/stageddeps.food.git")

  mockery::stub(dependency_table, "rec_checkout_internal_deps", mock_rec_checkout_internal_deps(TESTS_GIT_REPOS))
  capture.output(dep_table <- dependency_table(repo_dir, ref = "fixgarden@main")) # capture.output to make silent

  mockery::stub(check_downstream, "run_package_actions", function(pkg_actions, ...) {
    pkg_actions
  })

  expected_res <- data.frame(
    package_name = c(
      "stageddeps.elecinfra", "stageddeps.electricity",
      "stageddeps.food", "stageddeps.water", "stageddeps.house"
    ),
    stringsAsFactors = FALSE
  )
  # workaround because adding it directly into data.frame(...) does not work
  expected_res$actions <- list("install", "install", "install", "install", c("check", "install"))
  expect_equal(
    check_downstream(dep_table)[, c("package_name", "actions")],
    expected_res
  )

  # add only_tests = TRUE
  expected_res$actions <- list("install", "install", "install", "install", c("test", "install"))
  expect_equal(
    check_downstream(dep_table, only_tests = TRUE)[, c("package_name", "actions")],
    expected_res
  )
})

test_that("build_check_install works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")
  git2r::remote_set_url(repo_dir, name = "origin", url = "https://github.com/openpharma/stageddeps.food.git")

  mockery::stub(dependency_table, "rec_checkout_internal_deps", mock_rec_checkout_internal_deps(TESTS_GIT_REPOS))
  capture.output(dep_table <- dependency_table(repo_dir, ref = "fixgarden@main")) # capture.output to make silent

  mockery::stub(build_check_install, "run_package_actions", function(pkg_actions, ...) {
    pkg_actions
  })

  expected_res <- data.frame(
    package_name = c(
      "stageddeps.elecinfra", "stageddeps.electricity",
      "stageddeps.food", "stageddeps.water", "stageddeps.house", "stageddeps.garden"
    ),
    stringsAsFactors = FALSE
  )
  # workaround because adding it directly into data.frame(...) does not work
  expected_res$actions <- rep(list(c("build", "check", "install")), 6)
  expect_equal(
    build_check_install(dep_table)$pkg_actions[, c("package_name", "actions")],
    expected_res
  )
})


test_that("get_all_external_deps works", {

  # dummy dependency_structure object

  deps <- list(
    external = list(
      A = data.frame(type = c("Imports", "Suggests"), package = c("X", "Y")),
      B = data.frame(type = c("Depends"), package = "Z")
    ),
    upstream_deps = list(A = "B", B = character(0)),
    downstream_deps = list(B = "A", character(0))
  )

  internal_deps <- data.frame(package_name = c("A", "B"), type = c("current", "upstream"))

  x <- structure(
    list(
      project = NA,
      project_type = NA,
      current_pkg = NA,
      table = internal_deps,
      deps = deps,
      direction = "all"
    ),
    class = "dependency_structure"
  )


  available_packages <- data.frame(
    Package = c("T", "U", "V", "W", "X", "Y", "Z"),
    Depends = c(NA, NA, NA, "Q", "U", NA, "T"),
    Imports = c(NA, NA, NA, NA, NA, "U,V", NA),
    Suggests = c(NA, NA, NA, "S", NA, NA, "W"),
    LinkingTo = as.character(rep(NA, 7))
  )


  # These tests rely on a deterministic topological_sort function
  expect_equal(
    get_all_external_dependencies(x, available_packages = available_packages),
    c("U", "X", "V", "Y", "T", "Z")
  )

  # test from_internal_dependencies remove suggests)
  results <- get_all_external_dependencies(x, available_packages = available_packages, from_internal_dependencies = c("Depends", "Imports", "LinkingTo"))
  expect_equal(results, c("U", "X", "T", "Z"))

  # only take B's dependencies
  expect_equal(
    get_all_external_dependencies(x,
      package_list = "B",
      available_packages = available_packages
    ),
    c("T", "Z")
  )

  # test from_external_dependencies and check get warning for missing packages
  expect_warning(
    results <- get_all_external_dependencies(x,
      available_packages = available_packages,
      from_external_dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")
    ),
    "Cannot find information about package\\(s\\) Q, S check that options\\('repos'\\) contains expected repos"
  )

  # we just need Q before W, T before Z, U before X and U + V before Y
  expect_equal(results, c("Q", "W", "U", "X", "V", "Y", "T", "Z", "S"))

  # message and unsorted list if not all of Depends, Imports and LinkingTo are `from_external_dependencies`
  expect_message(results <- get_all_external_dependencies(x,
    package_list = "B",
    available_packages = available_packages,
    from_external_dependencies = c("Depends", "Imports")
  ),
  regexp = "Packages will not be ordered as this requires"
  )

  expect_equal(sort(results), c("T", "Z"))
})
