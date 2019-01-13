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
  theme_tda() +
    # add axis labels
    xlab("Feature appearance") +
    ylab("Feature disappearance")
}

#' Theme for Persistence Barcodes
#' 
#' A simple theme to clearly display persistent homology in
#' the form of persistence barcodes (aka topological barcodes).
#' 
#' @import ggplot2
#' @family TDA themes
#' @export
theme_barcode <- function() {
  theme_tda() +
    # add horizontal axis label
    xlab("Simplicial complex diameter") +
    # remove vertical axis
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
}
