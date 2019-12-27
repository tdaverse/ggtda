#' @title Barcodes
#'
#' @description Visualize persistence data in a barcode diagram.
#'

#' @details
#'
#' *Barcodes* or *barcode diagrams* are [vertical interval
#' plots](https://ggplot2.tidyverse.org/reference/geom_linerange.html) of
#' persistence data.
#' 

#' @template persistence-data
#' 

#' @section Barcodes:
#'
#'   Barcodes traditionally extend along the horizontal axis and are arranged
#'   vertically in order of group (e.g. dimension) and birth. They may also be
#'   transposed and juxtaposed with [persistence diagrams](persist). While
#'   topological features of different dimensions are usually plotted together
#'   in persistence diagrams, barcodes often separate segments corresponding to
#'   features of different dimension, by vertical grouping or by faceting.
#'
#' @template ref-carlsson2004
#' @template ref-carlsson2014
#' @template ref-chazal2017
#'
#' @name barcode
#' @import ggplot2
#' @family plot layers for persistence data
#' @seealso [ggplot2::layer()] for additional arguments.
#' @inheritParams ggplot2::layer
#' @param na.rm Logical:
#'   if `FALSE`, the default, `NA` lodes are not included;
#'   if `TRUE`, `NA` lodes constitute a separate category,
#'   plotted in grey (regardless of the color scheme).
#' @param ... Additional arguments passed to [ggplot2::layer()].
#' @param stat The The statistical transformation to use on the data.
#'   Defaults to `identity`; pass a string to override the default.
#' @example inst/examples/ex-barcode.R
#' @rdname barcode
#' @export
geom_barcode <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    geom = GeomBarcode,
    data = data,
    mapping = mapping,
    stat = stat,
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
GeomBarcode <- ggproto(
  "GeomBarcode", GeomSegment,
  
  required_aes = c("start", "end"),
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    if (! is.null(data$x) & ! is.null(data$xend) &
        is.null(data$start) & is.null(data$end)) {
      
      warning(
        "Substituting `x` and `xend` for missing persistence aesthetics ",
        "`start` and `end`."
      )
      
      # change `x` and `xend` to `start` and `end`
      data$start <- data$x
      data$end <- data$xend
    }
    
    # introduce numerical x-values in order to allow coordinate transforms
    data$x <- data$start
    data$xend <- data$end
    
    # introduce categorical -> integer y-values
    # in order of `group`, `x`, and `xend`
    grp <- if (is.null(data$group)) NA_character_ else data$group
    data$y <- interaction(
      grp, data$start, data$end,
      drop = TRUE, lex.order = TRUE
    )
    # re-distinguish duplicates
    data$y <- order(order(data$y))
    
    # return the pre-processed data set
    data
  },
  
  # generate graphical objects for each panel
  draw_panel = function(data, panel_params, coord) {
    
    # fill out necessary parameters for `GeomSegment`
    data$yend <- data$y
    
    # segment graphical object with new name
    grob <- GeomSegment$draw_panel(data, panel_params, coord)
    grob$name <- grid::grobName(grob, "geom_barcode")
    grob
  }
)
