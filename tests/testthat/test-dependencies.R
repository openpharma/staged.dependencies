# assumes that repo stageddeps.food is local
mock_rec_checkout_internal_deps <- function(repos_to_process, ...) {
  cat(paste0("Mocking rec_checkout_internal_deps", "\n"))
  expect_equal(repos_to_process, list(list(repo = "openpharma/stageddeps.food", host = "https://github.com")))

  local_pkgs <- c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food", "stageddeps.house", "stageddeps.garden", "stageddeps.water")
  local_repos <- data.frame(
    repo = paste0("openpharma/", local_pkgs),
    host = rep("https://github.com", 6),
    directory = file.path(TESTS_GIT_REPOS, local_pkgs),
    stringsAsFactors = FALSE
  )
  # stageddeps.food is local
  internal_deps <- data.frame(
    pkg = local_pkgs,
    repo = paste0("openpharma/", local_pkgs),
    host = rep("https://github.com", 6),
    branch = c("main", "main", "local (main)", "main", "main", "main"),
    stringsAsFactors = FALSE
  ) %>% dplyr::mutate(cache_dir = unlist(Map(get_repo_cache_dir, repo, host, local = grepl("^local ", branch))))
  # fs::dir_copy does not seem to be vectorized (although stated in the doc) -> use Map
  clear_cache()
  Map(fs::dir_copy, file.path(TESTS_GIT_REPOS, internal_deps$pkg), internal_deps$cache_dir)

  return(internal_deps %>% dplyr::select(repo, host, cache_dir, branch))
}

test_that("dependency_table works", {

  mockery::stub(dependency_table, 'rec_checkout_internal_deps', mock_rec_checkout_internal_deps)

  repo_dir <- file.path(TESTS_GIT_REPOS, "stageddeps.food")
  with_tmp_cachedir({
    expect_output(
      dep_table <- dependency_table(repo_dir, "main"),
      regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
    )

    expect_s3_class(dep_table, "dependency_structure")
    expect_equal(dep_table$current_pkg, "stageddeps.food")
    expect_equal(
      dep_table$table,
      data.frame(
        package_name = c("stageddeps.food", "stageddeps.elecinfra",
                         "stageddeps.electricity", "stageddeps.house", "stageddeps.garden",
                         "stageddeps.water"),
        type = factor(c("current", "upstream", "upstream", "downstream", "other", "other"),
                      levels = c("current", "upstream", "downstream", "other")),
        distance = c(0, 1, 2, 1, NA, NA),
        branch = c("local (main)", "main", "main", "main", "main", "main"),
        install_index = c(3, 1, 2, 5, 6, 4),
        stringsAsFactors = FALSE
      ) %>% dplyr::mutate(
        repo = paste0("openpharma/", package_name),
        host = rep("https://github.com", 6),
        cache_dir = unlist(Map(get_repo_cache_dir, repo, host, local = grepl("^local ", branch)))
      ) %>% dplyr::select(package_name, type, distance, branch, repo, host, cache_dir, install_index)
    )

    # check direction upstream only, should not matter since yamls agree with DESCRIPTION files
    expect_output(
      dep_table2 <- dependency_table(repo_dir, "main", direction = "upstream"),
      regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
    )
    expect_equal(dep_table[names(dep_table) != "direction"], dep_table2[names(dep_table2) != "direction"])
  })

})

test_that("dependency_table wih local_pkgs works", {
  copied_ecosystem <- tempfile("copied_ecosystem")
  fs::dir_copy(TESTS_GIT_REPOS, copied_ecosystem)

  local_pkgs <- c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food", "stageddeps.house", "stageddeps.garden", "stageddeps.water")
  local_repos <- data.frame(
    repo = paste0("openpharma/", local_pkgs),
    host = rep("https://github.com", 6),
    directory = file.path(copied_ecosystem, local_pkgs),
    stringsAsFactors = FALSE
  )

  expect_error(
    dependency_table(repo_dir, feature = "fixgarden@main", local_repos = local_repos),
    regexp = "must check out branch fixgarden@main", fixed = TRUE
  )

  git2r::checkout(file.path(copied_ecosystem, "stageddeps.garden"), branch = "fixgarden@main")
  expect_silent(
    dependency_table(repo_dir, feature = "fixgarden@main", local_repos = local_repos)
  )
})

test_that("check_yamls_consistent works", {
  copied_ecosystem <- tempfile("copied_ecosystem")
  fs::dir_copy(TESTS_GIT_REPOS, copied_ecosystem)

  local_pkgs <- c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food", "stageddeps.house", "stageddeps.garden", "stageddeps.water")
  local_repos <- data.frame(
    repo = paste0("openpharma/", local_pkgs),
    host = rep("https://github.com", 6),
    directory = file.path(copied_ecosystem, local_pkgs),
    stringsAsFactors = FALSE
  )

  # missing staged_dependencies.yaml in stageddeps.garden
  repo_dir <- file.path(copied_ecosystem, "stageddeps.food")
  expect_error(
    check_yamls_consistent(
      dependency_table(repo_dir, feature = "main", local_repos = local_repos)
    ),
    regexp = "for package stageddeps.garden", fixed = TRUE
  )

  git2r::checkout(file.path(copied_ecosystem, "stageddeps.garden"), branch = "fixgarden@main")
  check_yamls_consistent(
    dependency_table(repo_dir, feature = "fixgarden@main", local_repos = local_repos)
  )
})

test_that("plot.dependency_structure works", {
  mockery::stub(dependency_table, 'rec_checkout_internal_deps', mock_rec_checkout_internal_deps)

  repo_dir <- file.path(TESTS_GIT_REPOS, "stageddeps.food")
  expect_output(
    dep_table <- dependency_table(repo_dir, feature = "main", local_repos = local_repos),
    regexp = "Mocking rec_checkout_internal_deps", fixed = TRUE
  )
  plot_file <- tempfile("dep_plot", fileext = ".png")
  plot.dependency_structure(dep_table) %>% visNetwork::visSave(plot_file)
  expect_true(file.exists(plot_file))
  expect_true(file.info(plot_file)$size > 0) # expect non-empty
})
