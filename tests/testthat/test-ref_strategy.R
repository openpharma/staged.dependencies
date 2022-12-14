test_that("infer_ref_from_branch works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")

  # check that it infers the ref from branch name when no feature provided
  expect_equal(
    infer_ref_from_branch(repo_dir),
    "main"
  )

  # change branch and check that inferred ref matches new branch
  git2r::checkout(repo_dir, "fix1@main")
  expect_equal(
    infer_ref_from_branch(repo_dir),
    "fix1@main"
  )

  unlink(repo_dir, recursive = TRUE)
})

test_that("check_ref_consistency works", {
  repo_dir <- tempfile("stageddeps.food")
  fs::dir_copy(file.path(TESTS_GIT_REPOS, "stageddeps.food"), repo_dir)
  git2r::checkout(repo_dir, "main")

  # ref fix1@main matches existing branch fix1@main better than main, so a warning
  expect_warning(
    check_ref_consistency("fix1@main", repo_dir),
    regexp = "Branch fix1@main would match fix1@main", fixed = TRUE
  )

  # checked out branch "main" is consistent with feature "superfix@main",
  # it still returns the feature "superfix@main" since it was provided
  expect_silent(
    check_ref_consistency("superfix@main", repo_dir)
  )

  git2r::checkout(repo_dir, "fix1@main")

  # checked out branch fix1@main is not consistent with provided branch superfix@main
  expect_warning(
    check_ref_consistency("superfix@main", repo_dir),
    refexp = "fix1@main", fixed = TRUE
  )

  unlink(repo_dir, recursive = TRUE)
})

test_that("determine_ref works", {
  # feature1 is an existing branch
  expect_equal(
    determine_ref("feature1", data.frame(ref = c("main", "feature1"), type = "branch")),
    structure("feature1", type = "branch")
  )

  # branch "feature1@devel" does not exist, but "devel" exists
  expect_equal(
    determine_ref("feature1@devel", data.frame(ref = c("main", "devel", "feature1"), type = "branch")),
    structure("devel", type = "branch")
  )

  # branch "fix1@feature1@devel" exists
  expect_equal(
    determine_ref(
      "fix1@feature1@devel",
      data.frame(
        ref = c(
          "main", "devel", "feature1", "feature1@devel",
          "fix1@feature1@devel", "fix1"
        ),
        type = "branch"
      )
    ),
    structure("fix1@feature1@devel", type = "branch")
  )

  # branch "fix1@feature1@devel" does not exist, but "feature1@devel" exists
  expect_equal(
    determine_ref(
      "fix1@feature1@devel",
      data.frame(ref = c("main", "devel", "feature1", "feature1@devel", "fix1"), type = "branch")
    ),
    structure("feature1@devel", type = "branch")
  )

  # "devel" matches most closely
  expect_equal(
    determine_ref(
      "fix1@feature1@devel",
      data.frame(ref = c("main", "devel", "feature1", "fix1"), type = "branch")
    ),
    structure("devel", type = "branch")
  )


  # neither "feature1@release" nor "release" branches exist, so "main" is the fallback
  expect_equal(
    determine_ref("feature1@release", data.frame(ref = c("main", "devel"), type = "branch")),
    structure("main", type = "branch")
  )

  # tags are ignored if unmatched
  expect_equal(
    determine_ref(
      "devel",
      data.frame(ref = c("main", "devel", "v0.1", "v0.2"), type = c(rep("branch", 2), rep("tag", 2)))
    ),
    structure("devel", type = "branch")
  )

  # tags are matched
  expect_equal(
    determine_ref(
      "v0.2",
      data.frame(ref = c("main", "devel", "v0.1", "v0.2"), type = c(rep("branch", 2), rep("tag", 2)))
    ),
    structure("v0.2", type = "tag")
  )

  # if no tag/branch matched then fallback to main
  expect_equal(
    determine_ref(
      "v0.3",
      data.frame(ref = c("main", "devel", "v0.1", "v0.2"), type = c(rep("branch", 2), rep("tag", 2)))
    ),
    structure("main", type = "branch")
  )


  # tags and branch named the same, take the tag
  expect_equal(
    determine_ref(
      "devel",
      data.frame(ref = c("main", "devel", "v0.1", "devel"), type = c(rep("branch", 2), rep("tag", 2)))
    ),
    structure("devel", type = "tag")
  )

  # fallback "main" branch is not an available branch, so error
  expect_error(
    determine_ref("feature1@release", data.frame(ref = c("master", "devel"), type = "branch")),
    regexp = "at least one of 'feature1@release, release, main'", fixed = TRUE
  )

  # fallback "maaster" branch is an available branch, so use
  expect_equal(
    determine_ref("feature1@release", data.frame(ref = c("master", "devel"), type = "branch"), fallback_branch = "master"),
    structure("master", type = "branch")
  )

  # different branch separator
  expect_equal(
    determine_ref(
      "fix1#feature1#devel",
      data.frame(
        ref = c(
          "main", "devel", "feature1", "feature1#devel",
          "fix1@feature1@devel", "fix1"
        ),
        type = "branch"
      ),
      branch_sep = "#"
    ),
    structure("feature1#devel", type = "branch")
  )
})
