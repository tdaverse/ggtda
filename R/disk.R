#' @title Disks around points
#'
#' @description Center disks at points in a scatterplot
#'
#' @details
#'
#' A *ball* of radius \eqn{r} around a point \eqn{x} in Euclidean space consists
#' of all points whose distances from \eqn{x} are less than \eqn{r}. A ball in 2
#' dimensions is called a *disk*.
#'
#' The geometric objects of `GeomDisk` can be used to illustrate disk covers of
#' point clouds, in particular in the construction of simplicial filtrations. It
#' could be paired with a statistical transformation of `Stat*` that samples
#' points from a cloud.
#'
#' The convenience function `geom_disk()` produces a layer with the identity
#' statistical transformation.
#' 

#' @eval rd_sec_aesthetics(
#'   geom_disk = GeomDisk
#' )

#' @name disk
#' @import ggplot2
#' @family plot layers for point clouds
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical; ignored.
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param radius,diameter The (positive) radius or diameter used in the
#'   construction. Provide only one of these; if neither is provided, they
#'   default to zero.
#' @param segments The number of segments in the regular polygon that
#'   approximates each disk
#' @example inst/examples/ex-disk.r
#' @example inst/examples/ex-disk-simplicial-complex.R
# file.edit("inst/examples/ex-disk.r")
# file.edit("inst/examples/ex-disk-simplicial-complex.R")
NULL

#' @rdname ggtda-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDisk <- ggproto(
  "GeomDisk", Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = aes(colour = "NA", fill = "grey", alpha = .15,
                    linewidth = 0.5, linetype = 1),
  
  setup_params = function(data, params) {
    
    # harmonize parameters
    if (! is.null(params$diameter)) {
      if (! is.null(params$radius)) {
        warning("Pass a value to only one of `radius` or `diameter`; ",
                "`radius` value will be used.")
        params$diameter <- params$radius * 2
      } else {
        params$radius <- params$diameter / 2
      }
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    # nothing to draw
    if ((is.null(params$radius) && is.null(params$diameter)) ||
        params$segments == 0L) {
      data <- data[NULL, , drop = FALSE]
    }
    
    data
  },
  
  draw_panel = function(data, panel_params, coord,
                        radius = NULL, diameter = NULL,
                        segments = 60L) {
    
    # calculate a polygon that approximates a circle
    angles <- (0:segments) * 2 * pi / segments
    disk <- radius * cbind(cos(angles), sin(angles))
    disk <- as.data.frame(disk)
    names(disk) <- c("x.offset", "y.offset")
    
    # copy the circle to each point
    data <- transform(data, .id = seq(nrow(data)))
    data <- cbind(
      data[rep(seq(nrow(data)), each = nrow(disk)), ],
      disk[rep(seq(nrow(disk)), times = nrow(data)), ]
    )
    data <- transform(data,
                      x = x + x.offset, y = y + y.offset,
                      group = interaction(group, .id))
    data$group <- match(data$group, unique(data$group))
    data <- data[, setdiff(names(data), c("x.offset", "y.offset", ".id")),
                 drop = FALSE]
    
    # transform coordinates (after all geometric calculations are done)
    data <- coord$transform(data, panel_params)
    
    # create graphical objects
    grob <- grid::polygonGrob(
      data$x, data$y,
      default.units = "native",
      id = data$group,
      gp = grid::gpar(
        col = data$colour, 
        fill = alpha(data$fill, data$alpha), 
        lwd = (data$linewidth %||% data$size) * .pt,
        lty = data$linetype
      )
    )
    grob$name <- grid::grobName(grob, "geom_disk")
    grob
  },
  
  # https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/
  non_missing_aes = "size",
  rename_size = TRUE
)

#' @rdname disk
#' @export
geom_disk <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      na.rm = FALSE,
                      radius = NULL, diameter = NULL,
                      segments = 60L,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDisk,
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
