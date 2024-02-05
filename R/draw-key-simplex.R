#' @title Key glyphs for topological visualizations
#'
#' @description The `simplex` key is equivalent to the `polygon` key except that
#'   it renders triangles rather than squares. This evokes the triangularity
#'   of simplices and distinguishes plots from plots of arbitrary polygons.
#'
#' @name draw_key
#' @import ggplot2
#' @inheritParams ggplot2::draw_key
#' @example inst/examples/ex-draw-key-simplex.R

# file.edit("inst/examples/ex-draw-key-simplex.R")

draw_key_simplex <- function(data, params, size) {
  
  if (is.null(data$linewidth)) {
    data$linewidth <- 0.5
  }
  
  lwd <- min(data$linewidth, min(size) / 4)
  
  grid::polygonGrob(
    # equilateral triangle concentric with the unit square
    x = c(.25 * (2 - sqrt(3)), .5, .25 * (2 + sqrt(3))),
    y = c(.25, 1, .25),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill %||% "grey40", data$alpha),
      lty = data$linetype %||% 1,
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = params$lineend %||% "butt"
    ))
}
