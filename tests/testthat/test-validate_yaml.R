#---- Valid yaml ----
context("validate_yaml")
test_that("staged dep yaml without dependencies is valid ", {

  text <- "---
  downstream_repos:

  upstream_repos:

  current_repo:
    repo: test/test
    host: https://github.com
  "
  expect_silent(validate_staged_deps_yaml(yaml::read_yaml(text = text)))
})

context("validate_yaml")
test_that("staged dep yaml allows additional fields beyond those required", {

  text <- "---
  downstream_repos:
    - repo: test2/test
      host: https://github.com
      another_field : info
    - repo: test3/test
      host: https://github.com
  upstream_repos:
    - repo: test4/test
      host: https://github.com
  current_repo:
    repo: test/test
    host: https://github.com
    another_field : info

  another_field:
  "
  expect_silent(validate_staged_deps_yaml(yaml::read_yaml(text = text)))
})

#---- Invalid yaml ----
context("validate_yaml")
test_that("empty staged dep yaml file throws error", {

  text <- "---
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, it must contain fields upstream_repos, downstream_repos, current_repo"
  )

  text <- "
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, it must contain fields upstream_repos, downstream_repos, current_repo"
  )
})

context("validate_yaml")
test_that("staged dep yaml file missing expected fields throws error", {

  text <- "---
  hello
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, it must contain fields upstream_repos, downstream_repos, current_repo"
  )


  text <- "---
  downstream_repos:

  upstream_repos:
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, it must contain fields upstream_repos, downstream_repos, current_repo"
  )
})

context("validate_yaml")
test_that("staged dep yaml file without current_repo content throws error", {

  text <- "---
  downstream_repos:
    - repo: test2/test
      host: https://github.com
  upstream_repos:

  current_repo:

  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo cannot be empty"
  )
})

context("validate_yaml")
test_that("staged dep yaml file with current_repo as array throws error", {

  text <- "---
  upstream_repos:
  downstream_repos:
  current_repo:
    - repo: test/test
      host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo cannot be an array and must have entries repo, host"
  )
})

context("validate_yaml")
test_that("staged dep yaml file cannot contain non-character values", {

  text <- "---
  upstream_repos:
  downstream_repos:
    - repo: test2/test
      host: https://github.com
  current_repo:
    repo: true
    host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo must have non-array character values repo, host"
  )

  text <- "---
  upstream_repos:
  downstream_repos:
    - repo: test2/test
      host: https://github.com
  current_repo:
    repo: 456
    host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo must have non-array character values repo, host"
  )

  text <- "---
  upstream_repos:
  downstream_repos:
    - repo: -5.645
      host: https://github.com
  current_repo:
    repo: test/test
    host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field downstream_repos must have non-array character values repo, host"
  )

  text <- "---
  upstream_repos:
  downstream_repos:
    - repo: 546
      host: https://github.com
  current_repo:
    repo: test/test
    host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field downstream_repos must have non-array character values repo, host"
  )
})

context("validate_yaml")
test_that("staged dep yaml file missing repo or host throws error", {

  text <- "---
  upstream_repos:
  downstream_repos:
    - repo: test2/test
      host: https://github.com
  current_repo:
    repo: test/test
    not_host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo cannot be an array and must have entries repo, host"
  )

  text <- "---
  upstream_repos:
  downstream_repos:
    - repo: test2/test
      host: https://github.com
    - not_repo: test3/test
      host: https://github.com
  current_repo:
    repo: test/test
    host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field downstream_repos cannot be an array and must have entries repo, host"
  )
})

context("validate_yaml")
test_that("staged dep yaml with nesting/naming inside repo or host throws an error", {

  text <- "---
  downstream_repos:

  upstream_repos:

  current_repo:
    repo:
       - nested: test/test
    host: https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo must have non-array character values repo, host"
  )

  text <- "---
  downstream_repos:

  upstream_repos:

  current_repo:
    repo: test/test
    host:
      host_nested:
        https://github.com
  "
  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field current_repo must have non-array character values repo, host"
  )

  text <- "---
  downstream_repos:
    - repo: test2/test
      host: https://github.com
    - repo:
        - nested: test2/test
      host: https://github.com
  upstream_repos:

  current_repo:
    repo: test/test
    host: https://github.com
  "

  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field downstream_repos must have non-array character values repo, host"
  )

  text <- "---
  downstream_repos:
    - repo: test2/test
      host: https://github.com
    - repo: test2/test
      host:
        x: https://github.com
        repo: another_repo
  upstream_repos:

  current_repo:
    repo: test/test
    host: https://github.com
  "

  expect_error(
    validate_staged_deps_yaml(yaml::read_yaml(text = text)),
    regexp = "File  invalid, field downstream_repos must have non-array character values repo, host"
  )

})
