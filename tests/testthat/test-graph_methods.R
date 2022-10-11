# topological sort ----
test_that("topological sort throws error if circular relationship", {
  expect_error(topological_sort(
    list(n1 = "n3", n2 = "n2", n3 = c(""))
  ))

  expect_error(topological_sort(
    list(n1 = "n2", n2 = "n1")
  ))

  expect_error(topological_sort(
    list(n1 = "n2", n2 = "n3", n3 = c("n4", "n1"), n4 = c())
  ))
})

test_that("topological sort correctly sorts graph with no branching", {
  sorted_deps <- topological_sort(
    list(
      n1 = "n2",
      n2 = "n4",
      n3 = "n5",
      n4 = "n3",
      n5 = c()
    )
  )
  expect_equal(sorted_deps, c("n5", "n3", "n4", "n2", "n1"))
})

test_that("topological sort correctly sorts disconnected graph", {
  sorted_deps <- topological_sort(
    list(
      n1 = "n2",
      n2 = "n4",
      n3 = "n5",
      n4 = c(),
      n5 = c()
    )
  )

  expect_true(which(sorted_deps == "n2") < which(sorted_deps == "n1"))
  expect_true(which(sorted_deps == "n4") < which(sorted_deps == "n2"))
  expect_true(which(sorted_deps == "n5") < which(sorted_deps == "n3"))
  expect_equal(length(sorted_deps), 5)
  expect_equal(unique(sorted_deps), sorted_deps)
})

test_that("topological sort works with graph with branching", {
  sorted_deps <- topological_sort(
    list(
      n1 = "n3",
      n2 = c("n1", "n4"),
      n3 = c(),
      n4 = "n3"
    )
  )

  expect_equal(length(sorted_deps), 4)
  expect_equal(unique(sorted_deps), sorted_deps)
  expect_equal(sorted_deps[1], "n3")
  expect_equal(sorted_deps[4], "n2")
  expect_equal(sort(sorted_deps[2:3]), c("n1", "n4")) # ordering of these does not matter
})


test_that("topological sort works when parent occurs twice in graph", {
  sorted_deps <- topological_sort(
    list(
      n1 = c("n2", "n3", "n4", "n5"),
      n2 = c(),
      n3 = c("n4"),
      n4 = c(),
      n5 = c("n6"),
      n6 = c()
    )
  )
  expect_equal(sorted_deps, c("n2", "n4", "n3", "n6", "n5", "n1"))
})

test_that("adj_list_to_edge_df works", {
  # A -> B -> C -> D
  #      |    |
  #      |\-->\--> E
  #      |         ^
  #      \--> F --/
  expect_equal(
    adj_list_to_edge_df(
      list(A = "B", B = c("C", "F", "E"), C = c("D", "E"), D = c(), E = c(), F = c("E"))
    ) %>% dplyr::arrange(from, to),
    data.frame(
      from = c("A", "B", "B", "B", "C", "C", "F"),
      to = c("B", "C", "E", "F", "D", "E", "E"),
      stringsAsFactors = FALSE
    ) %>% dplyr::arrange(from, to)
  )
})

test_that("get_descendants works", {
  expect_equal(get_descendants(
    list(b = "a", c = "b", a = c()), c("b", "c")
  ), "a")
  expect_equal(get_descendants(
    list(b = "a", c = "b", a = c()), c("b")
  ), "a")
  expect_setequal(get_descendants(
    list(b = "a", c = "b", a = c()), c("c")
  ), c("a", "b"))

  # more complicated example
  # A -> B -> C -> D
  #      |    |
  #      |\-->\--> E
  #      |         ^
  #      \--> F --/]
  # one start_node
  expect_setequal(
    get_descendants(list(A = "B", B = c("C", "F", "E"), C = c("D", "E"), D = c(), E = c(), F = c("E")), "C"),
    c("D", "E")
  )
  # with two start_nodes
  expect_setequal(
    get_descendants(list(A = "B", B = c("C", "F", "E"), C = c("D", "E"), D = c(), E = c(), F = c("E")), c("B", "C")),
    c("D", "E", "F")
  )
})
