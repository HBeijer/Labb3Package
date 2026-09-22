#' Dijkstra algorithm
#'
#' Finds the shortest distance from one node to all other nodes
#' The function starts at the chosen node
#'
#' It checks the connected nodes and updates the shortest distance
#' This continues until all nodes have been visited
#'
#' @param graph A data frame with the columns v1, v2 and w
#' v1 and v2 are the nodes and w is the weight between them
#'
#'
#' @param init_node The node where the algorithm starts
#'
#' @return A vector with the shortest distance from init_node
#' to all nodes in the graph
#'
#'
#' @references
#' Wikipedia: Dijkstra's algorithm
#' \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#' @export
#'
#'
dijkstra <- function(graph, init_node){
  stopifnot("graph must be a data.frame" = is.data.frame(graph),
            "graph must contain exactly v1, v2 and w" = ncol(graph) == 3 && setequal(colnames(graph), c("v1", "v2", "w")),
            "v1, v2 and w must be numeric" = all(sapply(graph, is.numeric)),
            "init_node must be a numeric scalar" = is.numeric(init_node) && length(init_node) == 1,
            "init_node must exist in graph" = init_node %in% c(graph$v1, graph$v2))

  nodes <- sort(unique(c(graph$v1, graph$v2)))
  dist <- rep(Inf, length(nodes))
  names(dist) <- nodes

  dist[as.character(init_node)] <- 0
  current <- init_node

  visited <- rep(FALSE, length(nodes))
  names(visited) <- nodes

  while(sum(visited) < length(visited)) {

    work <- graph[graph$v1 == current, ]

    for(i in seq_len(nrow(work))) {
      neighbour <- work$v2[i]
      alt <- dist[as.character(current)] + work$w[i]

      if (alt < dist[as.character(neighbour)])
        dist[as.character(neighbour)] <- alt
    }

    visited[as.character(current)] <- TRUE

    candidate_dist <- dist
    candidate_dist[visited] <- Inf

    if(sum(visited) < length(visited))
      current <- nodes[which.min(candidate_dist)]
  }

  return(dist)
}
