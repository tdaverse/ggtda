#' The 1- and 2-skeletons of the Vietoris complex of a 2-dimensional point cloud
#'
#' The *Vietoris complex* of a point cloud is a simplicial complex consisting of a
#' simplex for each subset of points within a fixed diameter of each other. The
#' *1-skeleton* consists of all points (0-simplices) and edges between pairs
#' (1-simplices), and the *2-skeleton* additionally faces among all triples
#' (2-simplices), of a simplicial complex. Given `x` and `y` coordinates,
#' `stat_vietoris1()` encodes the edges of the Vietoris complex using `x`, `y`,
#' `xend`, and `yend` for `geom_segment()`, and `stat_vietoris2()` encodes the
#' faces using `x`, `y`, and `group` for `geom_polygon()`. Note that these stat
#' layers encode only the simplices of fixed dimension; to render the 1- or
#' 2-skeleton, they can be combined with `geom_point()`.
#' 

#' @name vietoris
#' @import ggplot2
#' @family point cloud plot layers
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; ignored.
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data; defaults to
#'   `segment` in `geom_vietoris1()` and to `polygon` in `geom_vietoris2`. Pass
#'   a string to override the default.
#' @param diameter A positive number; the distance between points at which
#'   segments will not be included.
#' @example inst/examples/ex-vietoris.R

#' @rdname vietoris
#' @export
stat_vietoris1 <- function(mapping = NULL,
                          data = NULL,
                          geom = "segment",
                          position = "identity",
                          na.rm = FALSE,
                          diameter = Inf,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  layer(
    stat = StatVietoris1,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      diameter = diameter,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatVietoris1 <- ggproto(
  "StatVietoris1", Stat,
  
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
                           diameter = Inf) {
    
    # indices of pairs of data points that are within `diameter` of each other
    edges <- proximate_pairs(data, diameter)
    edges <- t(edges)[c("a", "b"), ]
    
    # data frame of edges' starting and ending coordinates
    res <- data.frame(
      x = data$x[edges["a", , drop = TRUE]],
      y = data$y[edges["a", , drop = TRUE]],
      xend = data$x[edges["b", , drop = TRUE]],
      yend = data$y[edges["b", , drop = TRUE]]
    )
    
    # return the edges data
    res
  }
)

#' @rdname vietoris
#' @export
stat_vietoris2 <- function(mapping = NULL,
                           data = NULL,
                           geom = "polygon",
                           position = "identity",
                           na.rm = FALSE,
                           diameter = Inf,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    stat = StatVietoris2,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      diameter = diameter,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatVietoris2 <- ggproto(
  "StatVietoris2", Stat,
  
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
                           diameter = Inf) {
    
    # indices of pairs of data points that are within `diameter` of each other
    edges <- as.data.frame(proximate_pairs(data, diameter))
    
    # indices of triples of data points having diameter less than `diameter`
    faces <- merge(
      edges,
      transform(edges, b = a, c = b, a = NULL),
      by = "b", all = FALSE,
      sort = FALSE
    )
    faces <- t(as.matrix(faces))[c("a", "b", "c"), ]
    
    # data frame of faces' perimeter coordinates
    data <- data.frame(
      x = data$x[as.vector(faces)],
      y = data$y[as.vector(faces)],
      group = rep(1:ncol(faces), each = 3)
    )
    
    # return the faces data
    data
  }
)

proximate_pairs <- function(data, diameter) {
  distances <- as.matrix(stats::dist(data[, c("x", "y")]))
  pairs <- which(distances < diameter & upper.tri(distances), arr.ind = TRUE)
  dimnames(pairs) <- list(NULL, c("a", "b"))
  pairs
}
