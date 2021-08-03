#'
#' Topological graph sort
#'
#' Graph is a list which for each node contains a vector of child nodes
#' in the returned list, parents appear before their children.
#'
#' Implementation of Kahn algorithm with a modification to maintain the order of input elements.
#'
#' @param graph (named `list`) list with node vector elements mapping from
#'   child to its parents (upstream dependencies)
#' @return vector listing parents before children
#' @examples
#'   staged.dependencies:::topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
#'   staged.dependencies:::topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
#'   staged.dependencies:::topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
#' \dontrun{
#'   # cycle
#'   topological_sort(list(A = c("B"), B = c("C", "A"), C = c()))
#' }
topological_sort <- function(graph) {
  # compute in-degrees
  in_degrees <- list()
  for (node in names(graph)) {
    in_degrees[[node]] <- 0
    for (to_edge in graph[[node]]) {
      in_degrees[[to_edge]] <- 0
    }
  }

  for (node in graph) {
    for (to_edge in node) {
      in_degrees[[to_edge]] <- in_degrees[[to_edge]] + 1
    }
  }

  # sort
  visited <- 0
  sorted <- list()
  zero_in <- list()
  for (node in names(in_degrees)) {
    if (in_degrees[[node]] == 0) zero_in <- append(zero_in, node)
  }
  zero_in <- rev(zero_in)

  while (length(zero_in) != 0) {
    visited <- visited + 1
    sorted <- c(zero_in[[1]], sorted)
    for (edge_to in graph[[zero_in[[1]]]]) {
      in_degrees[[edge_to]] <- in_degrees[[edge_to]] - 1
      if (in_degrees[[edge_to]] == 0) {
        zero_in <- append(zero_in, edge_to, 1)
      }
    }
    zero_in[[1]] <- NULL
  }

  if (visited != length(in_degrees)) {
    stop("Dependency graph is not a directed acyclic graph. Cycles involving: ",
         paste0(setdiff(names(in_degrees), sorted), collapse = " "))
  } else {
    return(unlist(sorted))
  }
}

# get the descendants (all children) of node, given list mapping parent to children
get_descendants <- function(parents_to_children, node) {
  nodes_to_process <- c(node)
  descendants <- c()
  while (length(nodes_to_process) > 0) {
    cur_node <- nodes_to_process[[1]]
    nodes_to_process <- nodes_to_process[-1]

    descendants <- c(descendants, cur_node)
    children <- parents_to_children[[cur_node]]
    nodes_to_process <- union(nodes_to_process, setdiff(children, descendants))
  }

  setdiff(descendants, node)
}

# convert an adjacency list to a data.frame with from and to
# going from parents to children
adj_list_to_edge_df <- function(parents_to_children) {
  if (length(parents_to_children) == 0) {
    return(data.frame(from = character(), to = character()))
  }

  do.call(rbind, lapply(names(parents_to_children), function(node) {
    children <- parents_to_children[[node]]
    data.frame(
      from = if (length(children) > 0) node else character(0),
      to = unlist(children, recursive = FALSE), stringsAsFactors = FALSE
    )
  }))
}
