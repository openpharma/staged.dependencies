# # Preparation
# mkdir /tmp/test_ecosystem
# cd /tmp/test_ecosystem
# git clone git@github.com:openpharma/stageddeps.elecinfra.git
# git clone git@github.com:openpharma/stageddeps.electricity.git
# git clone git@github.com:openpharma/stageddeps.food.git
# git clone git@github.com:openpharma/stageddeps.garden.git
# git clone git@github.com:openpharma/stageddeps.house.git
# git clone git@github.com:openpharma/stageddeps.water.git
# tar -czvf /tmp/ecosystem.tgz -C "$(pwd)" .
#
# # Extraction
# untar("/tmp/ecosystem.tgz", list = TRUE)
# untar("/tmp/ecosystem.tgz", exdir = "/tmp/untarred_ecosystem")

TEST_GIT_REPOS <- "/tmp/test_ecosystem/"

test_that("infer_feature_from_branch works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TEST_GIT_REPOS, "stageddeps.food"), repo_dir)

  expect_equal(
    infer_feature_from_branch(NULL, repo_dir),
    "main"
  )
  expect_equal(
    infer_feature_from_branch("superfix@main", repo_dir),
    "superfix@main"
  )
  git2r::checkout(repo_dir, "fix1@main")
  expect_equal(
    infer_feature_from_branch(NULL, repo_dir),
    "fix1@main"
  )

  expect_warning(
    infer_feature_from_branch("superfix@main", repo_dir),
    refexp = "fix1@main", fixed = TRUE
  )

  unlink(repo_dir, recursive = TRUE)
})

test_that("determine_branch works", {
  expect_equal(
    determine_branch("feature1", c("main", "feature1")),
    "feature1"
  )

  expect_equal(
    determine_branch("feature1@devel", c("main", "devel", "feature1")),
    "devel"
  )

  expect_equal(
    determine_branch(
      feature = "fix1@feature1@devel",
      available_branches = c("main", "devel", "feature1", "feature1@devel",
                             "fix1@feature1@devel", "fix1")
    ),
    "fix1@feature1@devel"
  )

  expect_equal(
    determine_branch(
      "fix1@feature1@devel",
      c("main", "devel", "feature1", "feature1@devel", "fix1")
    ),
    "feature1@devel"
  )

  expect_equal(
    determine_branch(
      "fix1@feature1@devel",
      c("main", "devel", "feature1", "fix1")
    ),
    "devel"
  )

  expect_equal(
    determine_branch("feature1@release", c("main", "devel")),
    "main"
  )

  expect_error(
    determine_branch("feature1@release", c("master", "devel")),
    regexp = "at least one of 'feature1@release, release, main'", fixed = TRUE
  )
})
