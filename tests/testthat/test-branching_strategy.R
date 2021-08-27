test_that("infer_feature_from_branch works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")

  # check that it infers the feature from branch name when no feature provided
  expect_equal(
    infer_feature_from_branch(NULL, repo_dir),
    "main"
  )

  # feature fix1@main matches existing branch fix1@main better than main, so a warning
  expect_warning(
    infer_feature_from_branch("fix1@main", repo_dir),
    regexp = "feature fix1@main would match fix1@main", fixed = TRUE
  )

  # checked out branch "main" is consistent with feature "superfix@main",
  # it still returns the feature "superfix@main" since it was provided
  expect_equal(
    infer_feature_from_branch("superfix@main", repo_dir),
    "superfix@main"
  )

  # change branch and check that inferred feature matches new branch
  git2r::checkout(repo_dir, "fix1@main")
  expect_equal(
    infer_feature_from_branch(NULL, repo_dir),
    "fix1@main"
  )

  # checked out branch fix1@main is not consistent with provided branch superfix@main
  expect_warning(
    infer_feature_from_branch("superfix@main", repo_dir),
    refexp = "fix1@main", fixed = TRUE
  )

  unlink(repo_dir, recursive = TRUE)
})

test_that("determine_branch works", {
  # feature1 is an existing branch
  expect_equal(
    determine_branch("feature1", c("main", "feature1")),
    "feature1"
  )

  # branch "feature1@devel" does not exist, but "devel" exists
  expect_equal(
    determine_branch("feature1@devel", c("main", "devel", "feature1")),
    "devel"
  )

  # branch "fix1@feature1@devel" exists
  expect_equal(
    determine_branch(
      feature = "fix1@feature1@devel",
      available_branches = c("main", "devel", "feature1", "feature1@devel",
                             "fix1@feature1@devel", "fix1")
    ),
    "fix1@feature1@devel"
  )

  # branch "fix1@feature1@devel" does not exist, but "feature1@devel" exists
  expect_equal(
    determine_branch(
      "fix1@feature1@devel",
      c("main", "devel", "feature1", "feature1@devel", "fix1")
    ),
    "feature1@devel"
  )

  # "devel" matches most closely
  expect_equal(
    determine_branch(
      "fix1@feature1@devel",
      c("main", "devel", "feature1", "fix1")
    ),
    "devel"
  )

  # neither "feature1@release" nor "release" branches exist, so "main" is the fallback
  expect_equal(
    determine_branch("feature1@release", c("main", "devel")),
    "main"
  )

  # fallback "main" branch is not an available branch, so error
  expect_error(
    determine_branch("feature1@release", c("master", "devel")),
    regexp = "at least one of 'feature1@release, release, main'", fixed = TRUE
  )
})

