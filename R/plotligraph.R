#' Manipulate \pkg{plotly} into making a 3D render of an \pkg{igraph} graph.
#'
#' Works by drawing a scatter3d of the vertices and tracing each edge with its
#' own scatter3d. Procs one RColorBrewer palette warning per edge, but don't
#' worry about it.
#'
#' @import plotly
#' @import igraph
#' @export
#' @param graph_obj `igraph` object
#' @param layout optional `igraph` layout matrix, preferably in 3 dimensions.
#' Will generate with `layout.fruchterman.reingold` and dim = 3 otherwise.
#' @param ... additional arguments passed to `plot_ly`.
#' @return plot_ly object
#' @examples
#' library("plotly")
#' library("igraph")
#' library("plotligraph")
#'
#' set.seed(1234)
#' graph <- barabasi.game(35)
#' E(graph)$name <- sample(letters, length(E(graph)), TRUE)
#' plotligraph(graph)
plotligraph <- function(graph_obj, layout = NA, ...) {
  axis_options <- list(showticklabels = FALSE,
                       showaxeslabels = FALSE,
                       title = "")              # These last two do nothing

  edge_df <- get_edge_df(graph_obj)
  vertices <- as.character(V(graph_obj))

  if (is.na(layout)) {
    layout <- layout.fruchterman.reingold(graph_obj, dim = 3)
  }

  links <- get_edge_coordinates(edge_df, layout, vertices)

  plot_ly(data.frame(layout), x = X1, y = X2, z = X3,
          type="scatter3d",
          mode="markers",
          hoverinfo = "text",
          text = get.edge.attribute(graph_obj, "name"),
          ...) %>%
    add_edge_traces(links) %>%
    layout(scene = list(xaxis = axis_options,
                        yaxis = axis_options,
                        zaxis = axis_options))
}

get_edge_df <- function(graph_obj) {
  edge_df <- get.data.frame(graph_obj, what = "edges")
  edge_df$from <- as.character(edge_df$from)
  edge_df$to <- as.character(edge_df$to)
  edge_df

}

get_edge_coordinates <- function(edge_df, layout, vertices) {
  # Retrieve each edge as a pair of coodinates in the geometry of `layout`
  pts <- list()
  for (row in 1:nrow(edge_df)) {
    indices <- c(which(vertices == edge_df[row, "from"]),
                 which(vertices == edge_df[row, "to"]))
    pts[[row]] <- rbind(layout[indices[1], ], layout[indices[2], ])
  }
  pts
}

add_edge_traces <- function(plot_obj, pts) {
  # Add edges to the plotly graph by adding one trace per connection. Grim.
  for (pt in pts) {
    plot_obj <- add_trace(plot_obj,
                          data = data.frame(pt), x = X1, y = X2, z = X3,
                          type = "scatter3d",
                          hoverinfo = "none",
                          mode = "lines",
                          color = "rgb(13,13,13)",
                          showlegend = FALSE)
  }
  plot_obj
}
