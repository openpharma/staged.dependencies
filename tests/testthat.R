library(testthat)
library(staged.dependencies)

test_check("staged.dependencies", reporter = JunitReporter$new(file = "junit-result.xml"))
