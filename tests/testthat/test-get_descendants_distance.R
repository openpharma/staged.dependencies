test_that("get_descendants_distance with no descendants returns dataframe with 0 rows", {
  expect_equal(nrow(get_descendants_distance(NULL, starting_node = "A")), 0)
  expect_equal(nrow(get_descendants_distance(list(), starting_node = "A")), 0)
  expect_equal(nrow(get_descendants_distance(list(B = list("A")), starting_node = "A")), 0)
})


test_that("get_descendants_distance takes minimum distance between two nodes", {
  parents_to_children <- list(
    A = list("C"),
    B = list("A", "C"),
    C = list()
  )

  result <- get_descendants_distance(parents_to_children, starting_node = "B")
  expect_equal(result$id, c("A", "C"))
  expect_equal(result$distance, c(1, 1))
})


test_that("get_descendants_distance only includes descendants", {
  parents_to_children <- list(
    A = list("C"),
    B = list("A", "C"),
    C = list(),
    D = list("E"),
    E = list()
  )
  result <- get_descendants_distance(parents_to_children, starting_node = "B")
  expect_equal(result$id, c("A", "C"))
  expect_equal(result$distance, c(1, 1))

  result <- get_descendants_distance(parents_to_children, starting_node = "D")
  expect_equal(result$id, "E")
  expect_equal(result$distance, 1)
})

test_that("get_descendants_distance handles minimum distances greater than 1", {
  parents_to_children <- list(
    A = list("B"),
    B = list("C", "D"),
    C = list("E", "F"),
    D = list("G"),
    E = list("F", "G"),
    F = list(),
    G = list()
  )

  result <- get_descendants_distance(parents_to_children, starting_node = "A")
  expect_equal(result$id, c("B", "C", "D", "E", "F", "G"))
  expect_equal(result$distance, c(1, 2, 2, 3, 3, 3))
})


test_that("get_descendants_distance node names can include symbols", {
  parents_to_children <- list(
    `A @ A` = list("C / D"),
    `B > C` = list("A @ A", "C / D"),
    `C / D` = list()
  )

  result <- get_descendants_distance(parents_to_children, starting_node = "B > C")
  expect_equal(result$id, c("A @ A", "C / D"))
  expect_equal(result$distance, c(1, 1))
})

test_that("get_descendants_distance works - another example", {
  # A -> B -> C -> D
  #      |    |
  #      |\-->\--> E
  #      |         ^
  #      \--> F --/
  expect_equal(
    get_descendants_distance(
      list(A = "B", B = c("C", "F", "E"), C = c("D", "E"), D = c(), E = c(), F = c("E")),
      "B"
    ) %>% dplyr::arrange(id),
    data.frame(
      id = c("C", "D", "E", "F"),
      distance = c(1, 2, 1, 1),
      stringsAsFactors = FALSE
    ) %>% dplyr::arrange(id)
  )
})
