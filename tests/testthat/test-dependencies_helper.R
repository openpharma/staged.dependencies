test_that("get_local_pkgs_from_config works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)

  with_tmp_cachedir({
    config_file <- file.path(get_storage_dir(), CONFIG_FILENAME)
    expect_null(get_local_pkgs_from_config())
    cat(c(
      "local_packages:",
      "- repo: openpharma/stageddeps.food",
      "  host: https://github.com",
      paste0("  directory: \"", fs::path_abs(repo_dir), "\"")
    ), sep = "\n", file = config_file)
    res <- get_local_pkgs_from_config()

    expect_equal(res, data.frame(
      repo = "openpharma/stageddeps.food", host = "https://github.com",
      directory = as.character(fs::path_abs(repo_dir)), stringsAsFactors = FALSE
    ))
  })

  unlink(repo_dir, recursive = TRUE)
})
