#' Plot Persistent Homology as a Flat Persistence Diagram
#' 
#' Plots a flat persistence diagram.
#' 
#' @import ggplot2
#' @family TDA plot layers
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' param na.rm Logical:
#'   if `FALSE`, the default, `NA` values are not included
#'   if `TRUE`, `NA` values constitute a separate category,
#'   plotted in grey (regardless of the color scheme)
#' @param ... additional arguments passed to [ggplot2::layer()]
#' @param geom The geometric object used to display the data.
#'   Defaults to `point`; pass a string to override the default.
#' @export
stat_flat <- function(mapping = NULL,
                      data = NULL,
                      geom = "point",
                      position = "identity",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    stat = StatFlat,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @usage NULL
#' @export
StatFlat <- ggproto(
  "StatFlat", Stat,
  
  required_aes = c("start", "end"),
  
  # statistical transformation into plot-ready data
  compute_panel = function(data, scales) {
    # use `start` and `end` as `x` and `y`, respectively
    data$end <- data$end - data$start
    data$x <- data$start
    data$y <- data$end
    
    # return the transformed data frame
    data
  }
)

#' Plot Persistent Homology as a Persistence Diagram
#' 
#' Plots a flat persistence diagram
#' 
#' @import ggplot2
#' @family TDA plot layers
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' param na.rm Logical:
#'   if `FALSE`, the default, `NA` values are not included
#'   if `TRUE`, `NA` values constitute a separate category,
#'   plotted in grey (regardless of the color scheme)
#' @param ... additional arguments passed to [ggplot2::layer()]
#' @param stat The statistical transformation used to display the data.
#'   Defaults to `identity`; another useful option is `flat`.
#'   Pass a string to override the default.
#' @export
geom_persist <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    geom = GeomPersist,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @usage NULL
#' @export
GeomPersist <- ggproto(
  "GeomPersist", GeomPoint,
  
  required_aes = c("start", "end"),
  
  # pre-process the dataset
  setup_data = function(data, params) {
    
    # convert to flat persistence coordinates
    data$x <- data$start
    data$y <- data$end
    data$start <- NULL
    data$end <- NULL
    
    # return pre-processed dataset
    data
  }
)