#' Providing electricity
#'
#' @export
get_electricity <- function() {
  stageddeps.elecinfra::get_elecinfra()
  print("Providing electricity")
}
