# Custom draw_key fun -----------------------------------------------------
## (similar to draw_key_polygon, w/o border)
draw_key_simplex <- function(data, params, size) {
  
  if (is.null(data$linewidth)) {
    data$linewidth <- 0.5
  }
  
  lwd <- min(data$linewidth, min(size) / 4)
  
  grid::rectGrob(
    width = unit(1, "npc") - unit(lwd, "mm"),
    height = unit(1, "npc") - unit(lwd, "mm"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill %||% "grey40", data$alpha),
      lty = data$linetype %||% 1,
      lwd = lwd * .pt,
      linejoin = params$linejoin %||% "mitre",
      lineend = params$lineend %||% "butt"
    ))
}