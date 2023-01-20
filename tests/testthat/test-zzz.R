test_that("setup_storage_dir works", {
  # we modify the storage dir to TESTS_STORAGE_DIR for the tests in `testthat.R`, so check it agrees
  expect_equal(
    get_storage_dir(), TESTS_STORAGE_DIR
  )
  expect_equal(
    get_packages_cache_dir(),
    file.path(TESTS_STORAGE_DIR, "packages_cache")
  )

  # modify storage directory to a new random one
  new_storage_dir <- tempfile("random_storage_dir")
  set_storage_dir(new_storage_dir)
  on.exit({setup_storage_dir(TESTS_STORAGE_DIR)}, add = TRUE)
  setup_storage_dir(new_storage_dir)

  expect_equal(get_storage_dir(), new_storage_dir)
  expect_setequal(
    list.files(new_storage_dir),
    c("config.yaml", "packages_cache")
  )
  expect_equal(
    get_packages_cache_dir(),
    file.path(new_storage_dir, "packages_cache")
  )
})

test_that("get/set_storage_dir", {
  dir <- tempdir()

  withr::with_options(list("staged.dependencies._storage_dir" = dir), {
    expect_equal(get_storage_dir(), dir)
  })

  set_storage_dir(dir)
  expect_equal(get_storage_dir(), dir)
})
