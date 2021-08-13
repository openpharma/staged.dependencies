#' Taking care of the house
#'
#' @export
get_house <- function() {
  stageddeps.water::get_water()
  stageddeps.food::get_food()
  stageddeps.electricity::get_electricity()
  print("Using electricity for fridge, fridge for food, and water to clean the house.")
}
