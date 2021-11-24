example_lockfile_text <-
  '{
      "R": {
        "Version": "4.1.0",
        "Repositories": [
          {
            "Name": "CRAN",
            "URL": "https://cran.rstudio.com"
          }
        ]
      },
      "Packages": {
        "R6": {
          "Package": "R6",
          "Version": "2.5.1",
          "Source": "Repository",
          "Repository": "CRAN",
          "Hash": "470851b6d5d0ac559e9d01bb352b4021"
        }
      }
    }'

example_lockfile_text_2 <-
  '{
      "R": {
        "Version": "4.0.3",
        "Repositories": [
          {
            "Name": "CRAN",
            "URL": "https://cran.rstudio.com"
          }
        ]
      },
      "Packages": {
        "R6": {
          "Package": "R6",
          "Version": "2.5.1",
          "Source": "Repository",
          "Repository": "CRAN",
          "Hash": "470851b6d5d0ac559e9d01bb352b4021"
        }
      }
    }'

test_that("get_renv_lock_from_repo_dir returns json lockfile if exists and profile is NULL", {
  withr::with_tempdir({
    write(example_lockfile_text, file = "renv.lock")
    x <- get_renv_lock_from_repo_dir(".")
    expect_equal(x, jsonlite::parse_json(example_lockfile_text))
  })
})

test_that("get_renv_lock_from_repo_dir returns json lockfile from non NULL profile", {
  withr::with_tempdir({
    write(example_lockfile_text, file = "renv.lock")
    fs::dir_create(fs::path_join(c(".", "renv", "profiles", "test")))
    write(example_lockfile_text_2, file = fs::path_join(c(".", "renv", "profiles", "test", "renv.lock")))
    x <- get_renv_lock_from_repo_dir(".", renv_profile = "test")
    expect_equal(x, jsonlite::parse_json(example_lockfile_text_2))
  })
})

test_that("get_renv_lock_from_repo_dir returns NULL if no lockfile" , {
  withr::with_tempdir({
    expect_null(get_renv_lock_from_repo_dir("."))
    expect_null(get_renv_lock_from_repo_dir(".", renv_profile = "test"))
    write(example_lockfile_text, file = "renv.lock")
    expect_null(get_renv_lock_from_repo_dir(".", renv_profile = "test"))
    fs::dir_create(fs::path_join(c(".", "renv", "profiles", "test")))
    write(example_lockfile_text_2, file = fs::path_join(c(".", "renv", "profiles", "test", "renv.lock")))
    expect_null(get_renv_lock_from_repo_dir(".", renv_profile = "other"))
  })
})

test_that("get_renv_lock_from_repo_dir give warning and returns NULL if cannot read json of lockfile", {
  withr::with_tempdir({
    write("This is not JSON", file = "renv.lock")
    expect_null(expect_warning(get_renv_lock_from_repo_dir(".")))
  })
})
