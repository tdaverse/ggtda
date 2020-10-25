#' @title Disks about data points and skeletons of Vietoris and Čech complexes
#'
#' @description Annotate 2-dimensional point clouds with TDA constructions.
#'
#' @details
#'
#' These plot layers are useful for exposition and education; they illustrate
#' constructions used by TDA methods but cannot be pipelined into those methods.
#'

#' @section Definitions:
#'   

#' A *ball* of radius \eqn{r} around a point \eqn{x} in Euclidean space consists
#' of all points whose distances from \eqn{x} are less than \eqn{r}.
#'
#' The *Vietoris complex* of a point cloud is a simplicial complex consisting of
#' a simplex for each subset of points within a fixed diameter of each other.
#' The *Čech complex* consists of a simplex for each subset that lies within a
#' circle of fixed diameter. (This means that the Čech complex depends on the
#' geometry of the ambient space containing the point cloud, while the Vietoris
#' complex depends only on the inter-point distances.)
#'
#' The *0-skeleton* of a complex consists of its vertices (0-simplices), the
#' *1-skeleton* additionally the edges between pairs of vertices (1-simplices),
#' and the *2-skeleton* additionally faces among triples of vertices
#' (2-simplices).
#' 

#' @section Layers:
#'   

#' `geom_face()` is a convenience geom that is equivalent to `geom_polygon()`
#' except that its default aesthetics are more appropriate for the overlapping
#' elements produced by the stat layers.
#'
#' Given `x` and `y` coordinates, `stat_vietoris1()` encodes the edges of the
#' Vietoris complex using `x`, `y`, `xend`, and `yend` for `geom_segment()`, and
#' `stat_vietoris2()` encodes the faces using `x`, `y`, and `group` for
#' `geom_polygon()`. The edges of a Čech complex are exactly those of the
#' Vietoris complex, so `stat_cech1()` is an alias for `stat_vietoris1()`, while
#' `stat_cech2()` encodes the faces of the Čech complex in the same way as
#' `stat_vietoris2()` those of the Vietoris complex. Note that these stat layers
#' encode only the simplices of fixed dimension; to render the 1- or 2-skeleton,
#' they can be combined with `geom_vietoris0()` or `geom_cech0()`, which are
#' aliases for [ggplot2::stat_identity()] that default to
#' [ggplot2::geom_point()].
#' 

#' @template ref-chazal2017
#' 

#' @name simplicial-complex
#' @import ggplot2
#' @family plot layers for point clouds
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; ignored.
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param geom The geometric object to use display the data; defaults to
#'   `segment` in `stat_vietoris1()` and to `face` in `stat_vietoris2`. Pass a
#'   string to override the default.
#' @param radius A positive number; the radius of the disk to render around each
#'   point or to determine simplices from a point cloud.
#' @param diameter A positive number; the diameter of the disk to render around
#'   each point or to determine simplices from a point cloud.
#' @param segments The number of segments to be used in drawing each disk.
#' @example inst/examples/ex-simplicial-complex.R

#' @rdname simplicial-complex
#' @export
stat_disk <- function(mapping = NULL,
                      data = NULL,
                      geom = "face",
                      position = "identity",
                      na.rm = FALSE,
                      radius = NULL, diameter = NULL,
                      segments = 60,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDisk,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius, diameter = diameter,
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
  
  compute_panel = function(data, scales,
                           radius = NULL, diameter = NULL, segments = 60) {
    # handle disk dimension
    if ((is.null(radius) && is.null(diameter)) || segments == 0) {
      return(data[NULL, , drop = FALSE])
    }
    if (! is.null(diameter)) {
      if (! is.null(radius)) {
        warning("Pass a value to only one of `radius` or `diameter`; ",
                "`radius` value will be used.")
      } else {
        radius <- diameter / 2
      }
    }
    
    # calculate a polygon that approximates a circle
    angles <- (0:segments) * 2 * pi / segments
    disk <- radius * cbind(cos(angles), sin(angles))
    disk <- as.data.frame(disk)
    names(disk) <- c("x.offset", "y.offset")
    
    # copy the circle at each point
    disks <- tidyr::expand_grid(data[, c("x", "y")], disk)
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
    data = data,
    mapping = mapping,
    stat = StatVietoris0,
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
                           radius = NULL, diameter = NULL,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVietoris1,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius, diameter = diameter,
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
                           radius = NULL, diameter = NULL) {
    # handle disk dimension
    if (is.null(radius) && is.null(diameter)) {
      return(data[NULL, , drop = FALSE])
    }
    if (! is.null(radius)) {
      if (! is.null(diameter)) {
        warning("Pass a value to only one of `radius` or `diameter`; ",
                "`diameter` value will be used.")
      } else {
        diameter <- radius * 2
      }
    }
    
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
                           geom = "face",
                           position = "identity",
                           na.rm = FALSE,
                           radius = NULL, diameter = NULL,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVietoris2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius, diameter = diameter,
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
                           radius = NULL, diameter = NULL) {
    # handle disk dimension
    if (is.null(radius) && is.null(diameter)) {
      return(data[NULL, , drop = FALSE])
    }
    if (! is.null(radius)) {
      if (! is.null(diameter)) {
        warning("Pass a value to only one of `radius` or `diameter`; ",
                "`diameter` value will be used.")
      } else {
        diameter <- radius * 2
      }
    }
    
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
    faces <- t(as.matrix(faces))[c("a", "b", "c"), , drop = FALSE]
    
    # data frame of faces' perimeter coordinates
    res <- data.frame(
      x = data$x[as.vector(faces)],
      y = data$y[as.vector(faces)],
      group = rep(seq_len(ncol(faces)), each = 3)
    )
    
    # return the faces data
    res
  }
)

#' @rdname simplicial-complex
#' @export
stat_cech0 <- stat_vietoris0

#' @rdname simplicial-complex
#' @export
stat_cech1 <- stat_vietoris1

#' @rdname simplicial-complex
#' @export
stat_cech2 <- function(mapping = NULL,
                       data = NULL,
                       geom = "face",
                       position = "identity",
                       na.rm = FALSE,
                       radius = NULL, diameter = NULL,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCech2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius, diameter = diameter,
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
                           radius = NULL, diameter = NULL) {
    # handle disk dimension
    if (is.null(radius) && is.null(diameter)) {
      return(data[NULL, , drop = FALSE])
    }
    if (! is.null(radius)) {
      if (! is.null(diameter)) {
        warning("Pass a value to only one of `radius` or `diameter`; ",
                "`diameter` value will be used.")
      } else {
        diameter <- radius * 2
      }
    }
    
    # indices of triples of data points that are within `diameter` of some point
    faces <- proximate_triples(data, diameter)
    faces <- t(as.matrix(faces))[c("a", "b", "c"), , drop = FALSE]
    
    # data frame of faces' perimeter coordinates
    data <- data.frame(
      x = data$x[as.vector(faces)],
      y = data$y[as.vector(faces)],
      group = rep(seq_len(ncol(faces)), each = 3)
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
  # shed too-distant pairs
  dists <- dists[dists$d < diameter, , drop = FALSE]
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
  # semiperimeters
  triples$s <- .5 * (triples$d_ab + triples$d_bc + triples$d_ac)
  # circumdiameters
  triples$cd <- .5 * triples$d_ab * triples$d_bc * triples$d_ac / sqrt(
    triples$s * (triples$s - triples$d_ab) *
      (triples$s - triples$d_bc) * (triples$s - triples$d_ac)
  )
  # squares of longest sides
  triples$m <- pmax(triples$d_ab, triples$d_bc, triples$d_ac)^2
  # sum of squares of remaining sides
  triples$n <- triples$d_ab^2 + triples$d_bc^2 + triples$d_ac^2 - triples$m
  # when largest angles obtuse, longest side lengths; otherwise, circumdiameters
  triples$diam <- ifelse(
    triples$m > triples$n,
    pmax(triples$d_ab, triples$d_bc, triples$d_ac),
    triples$cd
  )
  triples <- triples[triples$diam < diameter, c("a", "b", "c"), drop = FALSE]
  rownames(triples) <- NULL
  triples
}

#' @rdname simplicial-complex
#' @export
geom_face <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFace,
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
GeomFace <- ggproto(
  "GeomFace", GeomPolygon,
  
  default_aes = aes(colour = "NA", fill = "grey", alpha = .15,
                    size = 0.5, linetype = 1)
)

acos_tol <- function(x, tol = sqrt(.Machine$double.eps)) {
  x <- ifelse(abs(x) > 1 + tol, x, pmin(pmax(x, -1), 1))
  acos(x)
}