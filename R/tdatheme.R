#' Theme for Persistent Homology Visualizations
#' 
#' A simple theme to clearly display persistent homology.
#' This minimalistic theme omits plot components that are
#' unnecessary in the context of persistent homology
#' (e.g. gridlines, background color).
#' This theme should get you close to a publication-quality
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