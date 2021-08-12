test_that("check_set_equal returns no error if sets are equal", {
  expect_silent(check_set_equal(character(0), character(0)))
  expect_silent(check_set_equal(c(4,5,6,4), c(6,5,4)))
  expect_silent(check_set_equal(c("A", "B"), c("B", "A")))
})

test_that("check_set_equal returns error message containing the set differences", {
  # note the escaping of \ in the regexp
  expect_error(check_set_equal(c("A", "B"), c("B")),
               regexp = "Sets do not agree, setdiff x \\\\ y is 'A', setdiff y \\\\ x is ''")

  expect_error(check_set_equal(1:5, 3:7),
               regexp = "Sets do not agree, setdiff x \\\\ y is '1, 2', setdiff y \\\\ x is '6, 7'")

})


test_that("pre_msg appears at the start of check_set_equal error message", {
  expect_error(check_set_equal(1:5, 3:7, pre_msg = "This is an error. "),
               regexp = "This is an error. Sets do not agree, setdiff x \\\\ y is '1, 2', setdiff y \\\\ x is '6, 7'")
})
