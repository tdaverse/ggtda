#' Obtain the 1-skeleton of a 2-dimensional point cloud
#'
#' Generates line segment data from point data encoding segments between all
#' points within a fixed distance of each other
#' 

#' @name skeleton
#' @import ggplot2
#' @family point cloud plot layers
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; ignored.
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data.
#'   Defaults to `segment`; pass a string to override the default.
#' @param stat The statistical transformation to use on the data.
#'   Defaults to `identity`; pass a string to override the default.
#' @param radius A positive number; the distance at which to exclude segments.
#' @example inst/examples/ex-skeleton.R

#' @rdname skeleton
#' @export
stat_skeleton <- function(mapping = NULL,
                          data = NULL,
                          geom = "segment",
                          position = "identity",
                          na.rm = FALSE,
                          radius = Inf,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatSkeleton,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatSkeleton <- ggproto(
  "StatSkeleton", Stat,
  
  required_aes = c("x", "y"),
  
  # pre-process of the parameters
  setup_params = function(data, params) {
    
    # return the pre-processed parameters
    params
  },
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # return the pre-processed data set
    data
  },
  
  # statistical transformation into plot-ready data
  compute_panel = function(data, scales,
                           radius = Inf) {
    
    # get indices of pairs of data points that are within `radius` of each other
    skeleton <- which(
      as.matrix(dist(data[, c("x", "y")])) < radius,
      arr.ind = TRUE
    )
    
    # construct a new data frame of starting and ending coordinates for segments
    data <- data.frame(
      x = data$x[skeleton[, 1]],
      y = data$y[skeleton[, 1]],
      xend = data$x[skeleton[, 2]],
      yend = data$y[skeleton[, 2]]
    )
    
    # return the transformed data frame
    data
  }
)
