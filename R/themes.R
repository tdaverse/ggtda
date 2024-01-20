#' @title Themes for persistence data visualizations
#'
#' @description A simple theme to clearly display persistent homology in the
#'   form of persistence barcodes or diagrams.
#'
#' @name themes
#' @param vertical Logical; whether to plot a vertical axis. Defaults to
#'   `FALSE`; if `TRUE`, allows users to more easily count the number of
#'   features plotted in barcode; equivalent to `theme_persist()`.
#' @import ggplot2

#' @rdname themes
#' @export
theme_persist <- function() {
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
}

#' @rdname themes
#' @export
theme_barcode <- function(vertical = FALSE) {
  # allows users to count how many features are there in barcode
  if (vertical) {
    theme_persist()
  # default: same as theme_persist() without the vertical axis
  } else {
    theme(axis.line.x = element_line(colour = "black"),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  }
}
