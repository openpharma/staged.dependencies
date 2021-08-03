# cat pasted arguments + new line
cat_nl <- function(...) cat(paste0(paste(...), "\n"))
# cat_nl("fff", "gg")

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
cbind_handle_empty <- function(df, ...) {
  col_and_vals <- list(...)
  if (nrow(df) == 0) {
    col_and_vals <- lapply(col_and_vals, function(x) character(0))
  }
  cbind(df, do.call(data.frame, c(col_and_vals, stringsAsFactors = FALSE)))
}
# cbind_handle_empty(data.frame(col1 = character(0), col2 = character(0)), col3 = "hello3", col4 = "hello4")
# cbind_handle_empty(data.frame(col1 = c("h1", "h11"), col2 = c("h2", "h22")), col3 = "hello3", col4 = "hello4")

# we need these functions because R does not support tuple indices,
# e.g. lst[[c(host=.., repo=..)]] is not possible
hash_repo_and_host <- function(repo_and_host) {
  if (length(repo_and_host) == 0) {
    c()
  } else {
    paste0(repo_and_host$repo, " @ ", repo_and_host$host)
  }
}
# hash_repo_and_host(list())
# hash_repo_and_host(list(repo = "repo1", host = "host1"))
# hash_repo_and_host(list(repo = c("repo1", "repo2"), host = c("host1", "host2")))

unhash_repo_and_host <- function(hashed_repo_and_host) {
  repo_and_host <- strsplit(hashed_repo_and_host, " @ ", fixed = TRUE)
  list(
    repo = extract_str_field(repo_and_host, 1),
    host = extract_str_field(repo_and_host, 2)
  )
}
# unhash_repo_and_host(character(0))
# unhash_repo_and_host("repo1 @ host1")
# unhash_repo_and_host(c("repo1 @ host1", "repo2 @ host2"))

check_verbose_arg <- function(verbose) {
  stopifnot(0 <= verbose, verbose <= 2)
}

require_pkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Please install ", pkg)
    }
  }
}

# stopifnot(dir.exists(..)) does not print a useful error message,
# so use this function instead
check_dir_exists <- function(direc, prefix = "") {
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
      stop("File ", file_name , " invalid, field ", field_name,
        " cannot be an array and must have entries ", toString(expected_fields)
      )
    }
    # and are unnamed and character scalars
    lapply(expected_fields, function(x){
      if (!rlang::is_scalar_character(content[[x]]) || rlang::is_named(content[[x]])) {
        stop("File ", file_name , " invalid, field ", field_name,
          " must have non-array character values ", toString(expected_fields)
        )
      }
    })
  }


  # first check the required fields exist
  required_fields <- lapply(required_schema, "[[", "name")
  if (!all(required_fields %in% names(content))) {
    stop("File ", file_name , " invalid, it must contain fields ", toString(required_fields))
  }

  # next check the contents of the fields is as expected
  lapply(required_schema, function(field){

    # extract the contents for this field
    sub_content <- content[[field$name]]

    # check not NULL if required and exit if NULL and that's OK
    if (!field$nullable && is.null(sub_content)) {
      stop("File ", file_name , " invalid, field ", field$name, " cannot be empty")
    }
    if (is.null(sub_content)){
      return(invisible(NULL))
    }

    # if field is not array type check content is expected
    if (!field$array) {
      check_single_entry(sub_content, field$subfields, field$name)
    }
    else { # if field is array type - for each element of array check content is expected
      lapply(sub_content, check_single_entry, field$subfields, field$name)
    }
  })
  return(invisible(NULL))
}
