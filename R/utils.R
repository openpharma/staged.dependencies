# cat pasted arguments + new line
cat_nl <- function(...) {
  cat(paste0(paste(...), "\n"))
}
# cat_nl("fff", "gg")
#' Set staged.dependencies verbosity
#'
#' @description
#' Functions to set and remove the option parameter `verbose_level_staged.deps`.
#' It can assume integer values between `c(0, 1, 2)`. This will set this variable
#' as an option with [options()] and [getOption()].
#'
#' @inheritParams argument_convention
#'
#' @examples
#' verbose_sd_set(2)
#' verbose_sd_get() # 2, the inserted value
#' verbose_sd_rm()
#' verbose_sd_get() # 1, the default
#'
#' @export
#' @name verbose_sd_option
verbose_sd_set <- function(verbose = 1) {
  options("verbose_level_staged.deps" = verbose)
}
#' @name verbose_sd_option
#' @export
verbose_sd_get <- function() {
  ret <- getOption("verbose_level_staged.deps")
  if (is.null(ret)) {
    1 # Default
  } else {
    ret
  }
}
#' @export
#' @name verbose_sd_option
verbose_sd_rm <- function() {
  if (is.null(getOption("verbose_level_staged.deps"))) {
    stop("No verbose_level_staged.deps to remove in general environment.")
  } else {
    options("verbose_level_staged.deps" = NULL) # Reset
  }
}


# output message if verbose argument is at least required_verbose
message_if_verbose <- function(..., verbose = NULL, required_verbose = 1, is_equal = FALSE) {
  if (is.null(verbose)) {
    verb <- verbose_sd_get()
  }
  # Should it be verbose equal to required or >= ?
  if (moe_sd(verb, required_verbose, is_equal)) {
    message(...)
  }
}

# Helper fnc - major or equal
moe_sd <- function(verb, req_verb, is_equal) {
  if (isTRUE(is_equal)) {
    verb == req_verb
  } else {
    verb >= req_verb
  }
}

extract_str_field <- function(lst, field) {
  vapply(lst, function(x) x[[field]], character(1))
}

# check is single string
is_non_empty_char <- function(x) {
  length(x) == 1 && nchar(x) > 0 && is.character(x)
}
# is_non_empty_char("ffff")
# is_non_empty_char(3)
# is_non_empty_char("")
# is_non_empty_char(c("ff", "gg"))

# if df is empty, don't add any type
# cbind_handle_empty(data.frame(col1 = character(0), col2 = character(0), stringsAsFactors = FALSE),
#   col3 = "hello3", col4 = "hello4")
# cbind_handle_empty(data.frame(col1 = c("h1", "h11"), col2 = c("h2", "h22"), stringsAsFactors = FALSE),
#   col3 = "hello3", col4 = "hello4")
cbind_handle_empty <- function(df, ...) {
  col_and_vals <- list(...)
  if (nrow(df) == 0) {
    col_and_vals <- lapply(col_and_vals, function(x) character(0))
  }
  cbind(df, do.call(data.frame, c(col_and_vals, stringsAsFactors = FALSE)))
}

# we need these functions because R does not support tuple indices,
# e.g. lst[[c(host=.., repo=..)]] is not possible
# hash_repo_and_host(list())
# hash_repo_and_host(list(repo = "repo1", host = "host1"))
# hash_repo_and_host(list(repo = c("repo1", "repo2"), host = c("host1", "host2")))
hash_repo_and_host <- function(repo_and_host) {
  if (length(repo_and_host) == 0) {
    c()
  } else {
    paste0(repo_and_host$repo, " @ ", repo_and_host$host, " @ ", repo_and_host$subdir)
  }
}

# unhash_repo_and_host(character(0))
# unhash_repo_and_host("repo1 @ host1")
# unhash_repo_and_host(c("repo1 @ host1", "repo2 @ host2"))
unhash_repo_and_host <- function(hashed_repo_and_host) {
  repo_and_host <- strsplit(hashed_repo_and_host, " @ ", fixed = TRUE)
  list(
    repo = extract_str_field(repo_and_host, 1),
    host = extract_str_field(repo_and_host, 2),
    subdir = extract_str_field(repo_and_host, 3)
  )
}

check_verbose_arg <- function(verbose) {
  stopifnot(0 <= verbose, verbose <= 2)
}

check_direction_arg <- function(direction) {
  stopifnot(rlang::is_scalar_character(direction) && direction %in% c("upstream", "downstream", "all"))
}

check_direction_arg_deprecated <- function(direction) {
  if (length(direction) == 2 && all(c("upstream", "downstream") %in% direction)) {
    warning('c("upstream", "downstream") is deprecated - please use "all".')
    direction <- "all"
  }
  return(direction)
}


require_pkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("For this feature of staged.dependencies, please install R package ", pkg)
    }
  }
}

# stopifnot(dir.exists(..)) does not print a useful error message,
# so use this function instead
check_dir_exists <- function(direc, prefix = "") {
  stopifnot(is.character(direc))
  if (!dir.exists(direc)) {
    stop(prefix, "Directory ", direc, " does not exist.")
  }
}


# validate the contents of the yaml file (after conversion into R)
# return NULL if valid throw error if not
validate_staged_deps_yaml <- function(content, file_name = "") {
  # A simplified schema object to capture the schema for the yaml file
  # each entry of the list contains the top level field, with their name
  # whether they can be NULL and their subfields. If array is TRUE then each element
  # of the field should contain the listed subfields (as characters)
  # More complex schemas cannot yet be validated
  # Fields in addition to these are ignored and no error is shown
  required_schema <- list(
    list(name = "upstream_repos", subfields = c("repo", "host"), nullable = TRUE, array = TRUE),
    list(name = "downstream_repos", subfields = c("repo", "host"), nullable = TRUE, array = TRUE),
    list(name = "current_repo", subfields = c("repo", "host"), nullable = FALSE, array = FALSE)
  )

  # helper function to validate field values
  check_single_entry <- function(content, expected_fields, field_name) {
    # check the required contents exist
    if (!(all(expected_fields %in% names(content)))) {
      stop(
        "File ", file_name, " invalid, field ", field_name,
        " cannot be an array and must have entries ", toString(expected_fields)
      )
    }
    # and are unnamed and character scalars
    lapply(expected_fields, function(x) {
      if (!rlang::is_scalar_character(content[[x]]) || rlang::is_named(content[[x]])) {
        stop(
          "File ", file_name, " invalid, field ", field_name,
          " must have non-array character values ", toString(expected_fields)
        )
      }
    })
  }


  # first check the required fields exist
  required_fields <- lapply(required_schema, "[[", "name")
  if (!all(required_fields %in% names(content))) {
    stop("File ", file_name, " invalid, it must contain fields ", toString(required_fields))
  }

  # next check the contents of the fields is as expected
  lapply(required_schema, function(field) {
    # extract the contents for this field
    sub_content <- content[[field$name]]

    # check not NULL if required and exit if NULL and that's OK
    if (!field$nullable && is.null(sub_content)) {
      stop("File ", file_name, " invalid, field ", field$name, " cannot be empty")
    }
    if (is.null(sub_content)) {
      return(invisible(NULL))
    }

    # if field is not array type check content is expected
    if (!field$array) {
      check_single_entry(sub_content, field$subfields, field$name)
    } else { # if field is array type - for each element of array check content is expected
      lapply(sub_content, check_single_entry, field$subfields, field$name)
    }
  })
  return(invisible(NULL))
}

# check that two sets agree and throw an error message detailing difference otherwise
check_set_equal <- function(x, y, pre_msg = "", return_error = FALSE) {
  if (!setequal(x, y)) {
    err_msg <- paste0(
      pre_msg, "Sets do not agree, ",
      "setdiff x \\ y is '", toString(setdiff(x, y)), "'",
      ", setdiff y \\ x is '", toString(setdiff(y, x)), "'"
    )
    if (return_error) {
      return(err_msg)
    } else {
      stop(err_msg)
    }
  }
  if (return_error) {
    return(NULL)
  } else {
    return(invisible(NULL))
  }
}

# given paths, return names of packages located at these paths
get_pkg_names_from_paths <- function(paths) {
  unname(vapply(paths, function(path) desc::desc_get_field("Package", file = path), character(1)))
}

# get upstream repos and downstream repos according to yaml file in repo directory
# if yaml file does not exist, returns empty lists
get_yaml_deps_info <- function(repo_dir) {
  check_dir_exists(repo_dir, "deps_info: ")

  yaml_file <- file.path(repo_dir, STAGEDDEPS_FILENAME)
  if (file.exists(yaml_file)) {
    content <- yaml::read_yaml(yaml_file)
    validate_staged_deps_yaml(content, file_name = yaml_file)
    # fill in optional subdir field if missing
    if (is.null(content$current_repo$subdir)) content$current_repo$subdir <- "."
    for (i in seq_along(content$upstream_repos)) if (is.null(content$upstream_repos[[i]]$subdir)) content$upstream_repos[[i]]$subdir <- "."
    for (i in seq_along(content$downstream_repos)) if (is.null(content$downstream_repos[[i]]$subdir)) content$downstream_repos[[i]]$subdir <- "."
    content
  } else {
    list(
      upstream_repos = list(), downstream_repos = list(),
      # function() so it does not error immediately
      current_repo = function() stop("Directory ", repo_dir, " has no ", STAGEDDEPS_FILENAME)
    )
  }
}

error_if_stageddeps_inexistent <- function(project) {
  fpath <- normalize_path(file.path(project, STAGEDDEPS_FILENAME))
  if (!file.exists(fpath)) {
    stop("file ", STAGEDDEPS_FILENAME, " does not exist in project folder: not restoring anything")
  }
}

# normalizePath() is broken
normalize_path <- function(x) as.character(fs::path_abs(x))

# make a named list with names 'nm' and each entry being 'x'
rep_with_names <- function(x, nm) {
  stats::setNames(rep(x, times = length(nm)), nm = nm)
}
