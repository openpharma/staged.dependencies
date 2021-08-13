#' Taking care of the food
#'
#' @export
get_food <- function() {
  stageddeps.electricity::get_electricity()
  print("Producing food")
}
