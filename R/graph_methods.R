#' Topologically sorts nodes so that parents are listed before all their children
#'
#' @param child_to_parents  mapping from child to its parents (upstream dependencies)
#'
#' @return vector listing parents before children
#'
#' @examples
#' topological_sort <- staged.dependencies:::topological_sort
#'
#' all(topological_sort(list(
#' n1 = c(), n2 = c("n1"), n3 = c("n2"),
#' n4 = c("n2", "n3"))) == c("n1", "n2", "n3", "n4")
#' )
#' is.null(topological_sort(list()))
#'
#' \dontrun{
#' # cycle
#' topological_sort(list(
#'   n1 = c("n2"), n2 = c("n3", "n1"), n3 = c()
#' ))
#' }
topological_sort <- function(child_to_parents) {
  # depth-first search from children to parents, then output nodes in the order of finishing times
  ordering <- c()
  visiting <- list()

  treat_node <- function(node) {
    # cat_nl("Treating node '", node, "'")
    # Sys.sleep(1)
    # print(visiting)
    if (node %in% names(visiting)) {
      stop("Node ", node, " forms part of a cycle.")
    }
    visiting[[node]] <<- TRUE
    for (parent in setdiff(child_to_parents[[node]], ordering)) {
      treat_node(parent)
    }
    visiting[[node]] <<- NULL # remove
    ordering <<- c(ordering, node)
  }

  nodes_to_process <- names(child_to_parents)
  while (length(nodes_to_process) > 0) {
    treat_node(nodes_to_process[[1]])
    nodes_to_process <- setdiff(nodes_to_process, ordering)
  }

  ordering
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
