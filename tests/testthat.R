library(testthat)
library(staged.dependencies)

# # Preparation
# mkdir /tmp/test_ecosystem
# cd /tmp/test_ecosystem
# git clone git@github.com:openpharma/stageddeps.elecinfra.git
# git clone git@github.com:openpharma/stageddeps.electricity.git
# git clone git@github.com:openpharma/stageddeps.food.git
# git clone git@github.com:openpharma/stageddeps.garden.git
# git clone git@github.com:openpharma/stageddeps.house.git
# git clone git@github.com:openpharma/stageddeps.water.git
# tar -czvf /tmp/ecosystem.tgz -C /tmp/test_ecosystem .

# copy to package dir
# cp /tmp/ecosystem.tgz inst

unpack_ecosystem <- function(target_dir, ...) {
  cat("Unpacking test ecosystem to directory ", target_dir, "\n")
  tar_file <- system.file("inst/ecosystem.tgz", package = "staged.dependencies")
  # untar(tar_file, list = TRUE)
  untar(tar_file, exdir = target_dir)
}
# TEST_GIT_REPOS <- "/tmp/test_ecosystem/"
TEST_GIT_REPOS <- tempfile("test_ecosystem")
unpack_ecosystem(TEST_GIT_REPOS)


test_check("staged.dependencies")
