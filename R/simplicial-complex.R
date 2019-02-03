#' @title Skeletons of Vietoris and Čech complexes
#'
#' @description Annotate 2-dimensional point clouds with TDA constructions.
#'
#' @details
#'
#' These plot layers are useful for exposition and education; they illustrate
#' constructions used by TDA methods but cannot be pipelined into those methods.
#'
#' The *Vietoris complex* of a point cloud is a simplicial complex consisting of
#' a simplex for each subset of points within a fixed diameter of each other.
#' The *Čech complex* consists of a simplex for each subset that lies within a
#' circle of fixed diameter. (This means that the Čech complex depends on the
#' geometry of the ambient space containing the point cloud, while the Vietoris
#' complex depends only on the inter-point distances.)
#'
#' The *1-skeleton* of a complex consists of all points (0-simplices) and edges
#' between pairs (1-simplices), and the *2-skeleton* additionally faces among
#' triples (2-simplices), of the complex.
#'
#' Given `x` and `y` coordinates, `stat_vietoris1()` encodes the edges of the
#' Vietoris complex using `x`, `y`, `xend`, and `yend` for `geom_segment()`, and
#' `stat_vietoris2()` encodes the faces using `x`, `y`, and `group` for
#' `geom_polygon()`. The edges of a Čech complex are exactly those of the
#' Vietoris complex, so `stat_cech1()` is an alias for `stat_vietoris1()`, while
#' `stat_cech2()` encodes the faces of the Čech complex in the same way as
#' `stat_vietoris2()` those of the Vietoris complex. Note that these stat layers
#' encode only the simplices of fixed dimension; to render the 1- or 2-skeleton,
#' they can be combined with `geom_point()`.
#' 

#' @template ref-chazal2017
#' 

#' @name simplicial-complex
#' @import ggplot2
#' @family plot layers for point cloud
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; ignored.
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data; defaults to
#'   `segment` in `geom_vietoris1()` and to `polygon` in `geom_vietoris2`. Pass
#'   a string to override the default.
#' @param radius A positive number; the radius of the disk to render around each
#'   point.
#' @param segments The number of segments to be used in drawing each disk.
#' @param diameter A positive number; the distance between points at which
#'   segments will not be included.
#' @example inst/examples/ex-simplicial-complex.R

#' @rdname simplicial-complex
#' @export
stat_disk <- function(mapping = NULL,
                      data = NULL,
                      geom = "polygon",
                      position = "identity",
                      na.rm = FALSE,
                      radius = 0,
                      segments = 60,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    stat = StatDisk,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius,
      segments = segments,
      ...
    )
  )
}

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatDisk <- ggproto(
  "StatDisk", Stat,
  
  required_aes = c("x", "y"),
  
  default_aes = aes(colour = "NA", fill = "grey", alpha = .15,
                    size = 0.5, linetype = 1),
  
  compute_panel = function(data, scales,
                           radius = 0, segments = 60) {
    if (radius == 0 || segments == 0) return(data[NULL, ])
    
    # calculate a polygon that approximates a circle
    angles <- (0:segments) * 2 * pi / segments
    disk <- radius * cbind(cos(angles), sin(angles))
    disk <- as.data.frame(disk)
    names(disk) <- c("x.offset", "y.offset")
    
    # copy the circle at each point
    disks <- tidyr::crossing(data[, c("x", "y")], disk)
    data$.id <- 1:nrow(data)
    data <- merge(data, disks, by = c("x", "y"))
    data <- transform(data,
                      x = x + x.offset, y = y + y.offset,
                      group = interaction(group, .id))
    data$group <- match(data$group, unique(data$group))
    data <- data[, setdiff(names(data), c("x.offset", "y.offset", ".id"))]
    
    # return circles data
    data
  }
)

#' @rdname simplicial-complex
#' @export
stat_vietoris0 <- function(mapping = NULL,
                           data = NULL,
                           geom = "point",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    stat = StatVietoris0,
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

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatVietoris0 <- ggproto(
  "StatVietoris0", StatIdentity,
  
  required_aes = c("x", "y")
)

#' @rdname simplicial-complex
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
  
  default_aes = aes(alpha = .25),
  
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

#' @rdname simplicial-complex
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
  
  default_aes = aes(alpha = .1),
  
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
    faces <- merge(
      faces,
      transform(edges, c = b, b = NULL),
      by = c("a", "c"), all = FALSE,
      sort = FALSE
    )
    faces <- t(as.matrix(faces))[c("a", "b", "c"), ]
    
    # data frame of faces' perimeter coordinates
    res <- data.frame(
      x = data$x[as.vector(faces)],
      y = data$y[as.vector(faces)],
      group = rep(1:ncol(faces), each = 3)
    )
    
    # return the faces data
    res
  }
)

#' @rdname simplicial-complex
#' @export
stat_cech0 <- function(mapping = NULL,
                       data = NULL,
                       geom = "point",
                       position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
  layer(
    stat = StatCech0,
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

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
StatCech0 <- ggproto(
  "StatCech0", StatIdentity,
  
  required_aes = c("x", "y")
)

#' @rdname simplicial-complex
#' @export
stat_cech1 <- stat_vietoris1

#' @rdname simplicial-complex
#' @export
stat_cech2 <- function(mapping = NULL,
                       data = NULL,
                       geom = "polygon",
                       position = "identity",
                       na.rm = FALSE,
                       diameter = Inf,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
  layer(
    stat = StatCech2,
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
StatCech2 <- ggproto(
  "StatCech2", StatVietoris2,
  
  # statistical transformation into plot-ready data
  compute_panel = function(data, scales,
                           diameter = Inf) {
    
    # indices of triples of data points that are within `diameter` of some point
    faces <- as.data.frame(proximate_triples(data, diameter))
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

proximate_triples <- function(data, diameter) {
  # distances between pairs
  dists <- data.frame(
    a = rep(1:(nrow(data) - 1), rep((nrow(data) - 1):1)),
    b = unlist(lapply(2:nrow(data), function(k) k:nrow(data))),
    d = as.vector(stats::dist(data))
  )
  dists <- dists[dists$d < diameter, ]
  # distances among triples
  triples <- merge(
    dists,
    transform(dists, c = dists$b, b = dists$a, a = NULL),
    by = "b", suffixes = c("_ab", "_bc")
  )
  triples <- merge(
    triples,
    transform(dists, c = dists$b, b = NULL, d_ac = dists$d, d = NULL),
    by = c("a", "c")
  )
  # triples within `diameter` of each other -> circumdiameter
  triples$s <- (triples$d_ab + triples$d_bc + triples$d_ac) / 2
  triples$cd <- 2 * triples$d_ab * triples$d_bc * triples$d_ac / sqrt(
    triples$s * (triples$s - triples$d_ab) *
      (triples$s - triples$d_bc) * (triples$s - triples$d_ac)
  )
  # vectors between pairs
  triples <- transform(
    triples,
    x_ab = data$x[triples$b] - data$x[triples$a],
    y_ab = data$y[triples$b] - data$y[triples$a],
    x_bc = data$x[triples$c] - data$x[triples$b],
    y_bc = data$y[triples$c] - data$y[triples$b],
    x_ac = data$x[triples$c] - data$x[triples$a],
    y_ac = data$y[triples$c] - data$y[triples$a]
  )
  # inner products of vectors within triples
  triples <- transform(
    triples,
    d_a <- triples$x_ab * triples$x_ac + triples$y_ab * triples$y_ac,
    d_b = triples$x_ab * triples$x_bc + triples$y_ab * triples$y_bc,
    d_c = triples$x_ac * triples$x_bc + triples$y_ac * triples$y_bc
  )
  # angles among triples
  triples <- transform(
    triples,
    t_a = acos(triples$d_a / (triples$d_ab * triples$d_ac)),
    t_b = acos(triples$d_b / (triples$d_ab * triples$d_bc)),
    t_c = acos(triples$d_c / (triples$d_ac * triples$d_bc))
  )
  # if any angle is obtuse, longest side length; otherwise, circumdiameter
  triples$diam <- ifelse(
    pmax(triples$t_a, triples$t_b, triples$t_c) > pi/2,
    pmax(triples$d_ab, triples$d_bc, triples$d_ac),
    triples$cd
  )
  triples <- triples[triples$diam < diameter, c("a", "b", "c")]
  rownames(triples) <- NULL
  triples
}
