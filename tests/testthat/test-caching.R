# does not require internet access, mocks git remote operations

test_that("clear_cache", {

  create_cache <- function(path) {
    fs::dir_create(file.path(path, "A"))
    fs::dir_create(file.path(path, "A", "AA"))
    fs::file_create(file.path(path, "A", "AA", "temp.txt"))
    fs::dir_create(file.path(path, "B"))
    fs::file_create(file.path(path, "B", "temp.txt"))
  }

  # test default pattern (remove all)
  with_tmp_cachedir({
    create_cache(get_packages_cache_dir())
    clear_cache()
    expect_length(fs::dir_ls(get_packages_cache_dir(), recurse = TRUE), 0)
  })

  # test pattern which removes all directories
  with_tmp_cachedir({
    create_cache(get_packages_cache_dir())
    expect_message(clear_cache(pattern = "[AB]"), "Cache empty")
    expect_length(fs::dir_ls(get_packages_cache_dir(), recurse = TRUE), 0)
  })

  # test pattern which keeps some directories
  with_tmp_cachedir({
    create_cache(get_packages_cache_dir())
    expect_message(clear_cache(pattern = "[Aa]"), "Directories remaining in cache:\nB\n")
    expect_length(fs::dir_ls(get_packages_cache_dir(), recurse = TRUE), 2)
  })

})



test_that("rec_checkout_internal_deps works (with mocking checkout)", {

  # mock checkout_repo by copying the appropriate directory to the repo_dir directory
  mockery::stub(rec_checkout_internal_deps, 'checkout_repo', function(repo_dir, repo_url, select_ref_rule, ...) {
    repo_name <- basename(repo_url)
    repo_name <- substr(repo_name, 0, nchar(repo_name) - nchar(".git"))
    cat(paste0("Mocking checkout_repo for ", repo_name, "\n"))

    unlink(repo_dir, recursive = TRUE)
    fs::dir_copy(file.path(TESTS_GIT_REPOS, repo_name), repo_dir)

    available_refs <- available_references(repo_dir)
    selected_ref <- select_ref_rule(available_refs)
    # do not do actual checkout of branch

    return(list(dir = repo_dir, ref = selected_ref, accessible = TRUE))
  })

  output <- capture.output(res <- rec_checkout_internal_deps(
    list(list(repo = "openpharma/stageddeps.food", host = "https://github.com")),
    "fix1@main",
    direction = c("upstream"), local_repos = NULL, verbose = 0
  ))

  # check mocked functions were called in correct order
  # dput(output)
  expect_equal(
    output,
    c("Mocking checkout_repo for stageddeps.food",
      "Mocking checkout_repo for stageddeps.electricity",
      "Mocking checkout_repo for stageddeps.elecinfra")
  )

  # check result by comparing to ground-truth
  expect_true(is.data.frame(res))

  expect_setequal(
    res$repo,
    c("openpharma/stageddeps.food",
      "openpharma/stageddeps.electricity",
      "openpharma/stageddeps.elecinfra")
  )
  expect_setequal(
    res$host,
    c("https://github.com", "https://github.com", "https://github.com")
  )
  expect_setequal(
    res$cache_dir,
    file.path(get_packages_cache_dir(), c(
      "openpharma_stageddeps.food_c04cb0c69b298637f12bb39ac4f17f95",
      "openpharma_stageddeps.electricity_d99d4ffe828b509002243d240b2f4859",
      "openpharma_stageddeps.elecinfra_7300aa2e17982fff37d603654479c06d")
    )
  )
  expect_setequal(
    res$ref,
    c("fix1@main", "main", "main")
  )

  # todo: check when local_repos not null, direction is upstream and downstream
})

# todo: end2end test
# test_that("rec_checkout_internal_deps works (without mocking)", {
#
# })
# dependency_table(repo_dir, "fix1@main")
# internal_deps <- internal_deps %>% dplyr::arrange(repo)
# res1 <- res1 %>% dplyr::arrange(repo)
# expect_equal(
#   internal_deps,
#   res1
# )
#
# local_pkgs <- c("stageddeps.elecinfra", "stageddeps.electricity", "stageddeps.food", "stageddeps.house", "stageddeps.garden", "stageddeps.water")
# local_repos <- data.frame(
#   repo = paste0("openpharma/", local_pkgs),
#   host = rep("https://github.com", 6),
#   directory = file.path(TESTS_GIT_REPOS, local_pkgs),
#   stringsAsFactors = FALSE
# )
# res <- data.frame(
#   repo = paste0("openpharma/", local_pkgs),
#   host = rep("https://github.com", 6),
#   branch = c("main", "main", "local (main)", "main", "main", "main"),
#   stringsAsFactors = FALSE
# ) %>% dplyr::mutate(cache_dir = unlist(Map(get_repo_cache_dir, repo, host, local = grepl("^local ", branch)))) %>%
#   dplyr::select(repo, host, cache_dir, branch)


test_that("get_hashed_repo_to_dir_mapping works", {
  expect_equal(
    get_hashed_repo_to_dir_mapping(
      data.frame(
        repo = "openpharma/stageddeps.food",
        host = "https://github.com",
        directory = "dummy_dir",
        stringsAsFactors = FALSE
      )
    ),
    c(`openpharma/stageddeps.food @ https://github.com` = "dummy_dir")
  )
})

# whether git status is clean
git_status_clean <- function(repo_dir) {
  # from git2r:::print.git_status
  max(sapply(git2r::status(repo_dir), length)) == 0L
}

test_that("copy_local_repo_to_cachedir works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")

  #set config (needed for automation)
  git2r::config(git2r::repository(repo_dir), user.name = "github.action", user.email = "gh@action.com")

  # add some staged, unstaged and untracked files
  withr::with_dir(repo_dir, {
    cat("newcontent1", file = "README.md", append = TRUE)
    cat("newcontent2", file = "inexistentFile1.md")
    cat("newcontent3", file = "inexistentFile2.md")
    cat("newcontent4", file = "DESCRIPTION", append = TRUE)
    git2r::add(".", "README.md")
    git2r::add(".", "inexistentFile1.md")
  })

  # ckeck all files (from above) are added to the git commit in a local cache dir
  with_tmp_cachedir({
    res <- expect_message(
      copy_local_repo_to_cachedir(
        repo_dir,
        repo = "openpharma/stageddeps.food", host = "https://github.com",
        select_ref_rule = function(available_refs) {
          structure("main", type = "branch")
        },
        verbose = 2
      ),
      regexp = "Adding all of the following", fixed = TRUE
    )
    expect_true(startsWith(res$dir, file.path(get_packages_cache_dir(), "local_")))
    expect_true(file.exists(file.path(res$dir, "inexistentFile1.md")))
    expect_equal(res$ref, "local (main)")
    expect_true(git_status_clean(res$dir))
  })

  unlink(repo_dir, recursive = TRUE)
})
