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
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop("Please install ", pkg)
    }
  }
}

# todo: remove
# system2_succeed <- function(...) {
#   res <- system2(..., stdout = TRUE)
#   if (!is.null(attr(res, "status"))) {
#     # stop("Error running command '", toString(list(...)), "': ", toString(attr(res, "errmsg")))
#     stop("Error running command")
#   }
#   res
# }
# # system2_succeed("echo", "hh")
# # system2_succeed("echo11", "hh")
