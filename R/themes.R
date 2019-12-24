#' Theme for Persistent Homology Visualizations
#' 
#' A simple theme to clearly display persistent homology.
#' This minimalistic theme omits plot components that are
#' unnecessary in the context of persistent homology
#' (e.g. gridlines, background color).
#' The `theme_tda` function should get you close to a publication-quality
#' plot
#' 
#' @import ggplot2
#' @family TDA themes
#' @export
theme_tda <- function() {
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
}

#' Theme for Persistence Diagrams
#' 
#' A simple theme to clearly display persistent homology in
#' the form of flat persistence diagrams or diagonal persistence diagrams.
#' 
#' @import ggplot2
#' @family TDA themes
#' @export
theme_persist <- function() {
  # no real differences (vertical axis is needed)
  theme_tda()
}

#' Theme for Persistence Barcodes
#' 
#' A simple theme to clearly display persistent homology in
#' the form of persistence barcodes (aka topological barcodes).
#' 
#' @import ggplot2
#' @param vertical  defaults to `FALSE` (no vertical axis); if `TRUE`, allows
#'   users to more easily count the number of features plotted in barcode
#'   (if `vertical` equals `TRUE`, this function is equivalent to `theme_tda`)
#' @family TDA themes
#' @export
theme_barcode <- function(vertical = FALSE) {
  # allows users to count how many features are there in barcode
  if (vertical) {
    theme_tda()
  # default: same as theme_tda() without the vertical axis
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
