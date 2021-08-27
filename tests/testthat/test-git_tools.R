test_that("checkout_repo works", {
  # sodo: mock git clone etc so it works in automation (will be addressed in issue #33)
  # expect_silent(
  #   checkout_repo(tempfile(), "https://code.roche.com/maximilian_oliver.mordig/stageddeps.water",
  #                 function(...) "master", "ROCHE_GITLAB_PAT")
  # )
  # expect_silent(
  #   checkout_repo(tempfile(), "https://github.com/insightsengineering/test.nest",
  #                 function(...) "main", "GITHUB_PAT")
  # )

  # Sys.setenv(INCORRECT_TOKEN = "faadsads") # better to use withr::with_env
  # on.exit(Sys.unsetenv("INCORRECT_TOKEN"))
  # expect_error(
  #   checkout_repo(tempfile(), "https://code.roche.com/maximilian_oliver.mordig/stageddeps.water",
  #                 function(...) "main", "INCORRECT_TOKEN"),
  #   regexp = "INCORRECT_TOKEN", fixed = TRUE
  # )
  # expect_error(
  #   checkout_repo(tempfile(), "https://github.com/insightsengineering/test.nest",
  #                 function(...) "main", "INCORRECT_TOKEN"),
  #   regexp = "INCORRECT_TOKEN", fixed = TRUE
  # )
})
