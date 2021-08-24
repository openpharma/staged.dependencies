# This file is executed before the tests, see https://www.r-bloggers.com/2020/11/helper-code-and-files-for-your-testthat-tests/


# unpack ecosystem for packages ----

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
# cp /tmp/ecosystem.tgz tests/testthat/ecosystem

unpack_ecosystem <- function(target_dir, ...) {
  cat("Unpacking test ecosystem to directory", target_dir, "\n")
  tar_file <- test_path("ecosystem", "ecosystem.tgz")
  # untar(tar_file, list = TRUE)
  untar(tar_file, exdir = target_dir)
}
# TESTS_GIT_REPOS <- "/tmp/test_ecosystem/"
TESTS_GIT_REPOS <- tempfile("test_ecosystem")
unpack_ecosystem(TESTS_GIT_REPOS)

# check main branch checked out everywhere
for (dir in list.dirs(TESTS_GIT_REPOS, recursive = FALSE, full.names = TRUE)) {
  stopifnot(get_current_branch(dir) == "main")
}



# set storage dir ----
TESTS_STORAGE_DIR <- tempfile("stageddeps_storage")
setup_storage_dir(TESTS_STORAGE_DIR)


# helper functions ----

# execute code after setting a temporary cache directory for staged.dependencies
with_tmp_cachedir <- function(code) {
  old_storage_dir <- get_storage_dir()
  on.exit(setup_storage_dir(old_storage_dir), add = TRUE)
  new_storage_dir <- tempfile("storage_dir")
  setup_storage_dir(new_storage_dir)
  res <- force(code)
  unlink(new_storage_dir, recursive = TRUE)
  return(res)
}
