#' @title Persistence frontiers
#'
#' @description Visualize the frontiers of persistence data.
#'   

#' @details
#'
#' We call the *persistence frontier* the first level of the persistence
#' landscape (Bubenik, 2015) for each dimension of persistence data.
#' 

#' @template persistence-data
#'

#' @template ref-bubenik2015

#' @eval rd_sec_aesthetics(
#'   stat_frontier = StatFrontier,
#'   extra_note = paste0(
#'     "`start` and `end` are dropped during the statistical transformation."
#'   )
#' )

#' @eval rd_sec_computed_vars(
#'   stat = "frontier",
#'   "x,y" = "coordinates of segment endpoints of each frontier."
#' )

#' @name frontier
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @inheritParams persistence
#' @param geom The geometric object to use display the data; defaults to `line`.
#'   Pass a string to override the default.
#' @example inst/examples/ex-frontier.R
NULL

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatFrontier <- ggproto(
  "StatFrontier", Stat,
  
  required_aes = c("start", "end"),
  
  compute_group = function(data, scales,
                           diagram = "diagonal") {
    
    # first row (for aesthetics)
    first_row <- data[1L, setdiff(names(data), c("start", "end")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # Pareto frontier
    data <- pareto_persistence(data)
    data <- data[order(data$start), , drop = FALSE]
    if (! all(data$end == cummax(data$end))) {
      warning("`start` and `end` were not successfully anti-sorted.")
    }
    
    # points in cartesian coordinates
    data$x <- data$start
    data$y <- data$end
    
    # data frame of segment positions
    data <- data.frame(
      x = c(rep(data$start, each = 2), data$end[nrow(data)]),
      y = c(data$start[1], rep(data$end, each = 2))
    )
    
    # diagram transformation
    data <- diagram_transform(data, diagram)
    
    # return frontier data
    cbind(data, first_row)
  }
)

#' @rdname frontier
#' @export
stat_frontier <- function(mapping = NULL,
                          data = NULL,
                          geom = "line",
                          position = "identity",
                          diagram = "diagonal",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatFrontier,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diagram = diagram,
      na.rm = na.rm,
      ...
    )
  )
}

pareto_persistence <- function(data) {
  if ("rPref" %in% rownames(utils::installed.packages())) {
    pareto_persistence_rPref(data)
  } else {
    pareto_persistence_base(data)
  }
}

pareto_persistence_base <- function(data) {
  pd <- data[order(data$start, -data$end), ]
  pd[! duplicated(cummax(pd$end)), ]
}

pareto_persistence_rPref <- function(data) {
  rPref::psel(data, rPref::low("start") * rPref::high("end"))
}
