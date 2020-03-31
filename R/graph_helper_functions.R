#' Create one-mode igraph object from bipartite network data.
#' @export
create_graph_from_bipartite_data <-
  function(edge_list,
           node_attributes = NULL,
           node_id = "researcher_id",
           projection_to_extract = 2) {
    bi_graph <- graph_from_data_frame(edge_list, FALSE)
    V(bi_graph)$type <- bipartite_mapping(bi_graph)$type

    graph <- bipartite.projection(bi_graph)[[projection_to_extract]]

    if (!is.null(node_attributes))
      as_tbl_graph(graph) %>%
      left_join(node_attributes, by = c("name" = node_id))
    else
      graph
  }


filter_graph_by_component_size <-
  function(graph, min_component_size = 3) {
    comp <- components(graph)
    clusters_to_keep <- which(comp$csize >= min_component_size)
    print(paste0(
      comp$no,
      " components. Biggest: ",
      max(comp$csize),
      ". Keeping " ,
      length(clusters_to_keep),
      " components."
    ))

    filter_graph_by_ids(graph,
                        names(comp$membership)[comp$membership %in% clusters_to_keep])
  }

#' Filter graph by ids/ names.
#' @export
filter_graph_by_ids <-
  function(graph, ids) {
    graph %>%
      as_tbl_graph() %>%
      filter(name %in% ids)
  }

#' Plot graph with stress layout and optionally with legend and some statistics.
plot_graph_with_legend_and_stats <-
  function(g,
           layout = NULL,
           stats = NULL,
           legend_color_df = NULL,
           main = NULL) {
    par(mar = c(10, 0, 5, 0), xpd  = TRUE)

    if (is.null(layout))
      layout <- graphlayouts::layout_with_stress(g)

    plot(
      g,
      vertex.label = "",
      vertex.size = 1.5,
      layout = layout,
      edge.width = 1,
      vertex.frame.color = NA,
      main = main
    )
    if (!is.null(legend_color_df))
      legend(
        "bottomleft",
        legend = legend_color_df$big_field,
        col = legend_color_df$color,
        pch = 20,
        pt.cex	= 2,
        inset = c(0.2, -0.18)
      )

    #rect(0,-1.5,1,-1.1, col = "white", border = "black")
    if (is.null(stats))
      stats <- c(
        Nodes = length(V(g)),
        `Components (n>2)` = igraph::components(g)$no,
        Density = round(edge_density(g), 4)
      )
    text(0.05,
         seq(-1.1, -1.1 - (length(stats) - 1) * 0.05, by = -0.05),
         paste(names(stats), stats, sep = ": "),
         pos = 4)
  }