get_repo_access <- function(repo, host, token_envvar){

  host_type <- NULL
  if (host == "https://github.com") {
    host_type <- "github"
  } else if (host %in% c("https://gitlab.com", "https://code.roche.com")) {
    host_type = "gitlab"
  } else {
    return(NULL)
  }

  if (host_type == "github") {
    resp <- httr::GET(
      paste0(gsub(":\\/\\/", ":\\/\\/api.",host), "/repos/", repo),
      # `token` argument not working
      httr::add_headers(c(Authorization = paste("token", Sys.getenv(token_envvar))))
    )
  } else {
    # gitlab
    resp <- httr::GET(
      paste0(
        host, "/api/v4/projects/",
        utils::URLencode(repo, reserved = TRUE)
      ),
      httr::add_headers(c(Authorization = paste("Bearer", Sys.getenv(token_envvar))))
    )
  }
  return(resp)
}
