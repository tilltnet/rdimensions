#' Fastly create igraph object from bipartite data using data.table
#'
#' @param data `data.frame` with from and to column
#' @param edge_threshold `numeric` defining minimum edges for an edge to be included
#' @param skip_make_unique `logical`, set `TRUE` to include duplicated rows in `data`.
#' @export
fast_bi_igraph <-
  function(data, edge_threshold = 1300, skip_make_unique = FALSE) {
    dtx <- as.data.table(data)
    dtx <- dtx[!is.na(var), .SDcols = c(xid, var)]
    if(!skip_make_unique) {
      dtx <- dtx[, unique(.SD), by = xid]
    }
    dtx <- dtx[, .(from = rep(var, .N),
                   to = as.character(gl(.N, .N, labels = var))), by = xid]
    dtx <- dtx[, .(
      from,
      from_tmp = ifelse(from < to, from, to),
      to,
      to_tmp = ifelse(from > to, from, to)
    ), by = xid]
    dtx <- dtx[, .(xid,
                   from = from_tmp,
                   to = to_tmp)]
    dtx <- dtx[, unique(.SD), by = xid]
    el <- dtx[, .(weight = .N), .(from, to)]
    el_filtered <-
      el[weight > edge_threshold]
    el_filtered <-
      el_filtered[from != to]

    graph_from_data_frame(el_filtered, FALSE)
  }
