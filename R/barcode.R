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

#' @eval rd_sec_aesthetics(
#'   geom_barcode = GeomBarcode
#' )

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
# file.edit("inst/examples/ex-barcode.R")
NULL

#' @rdname ggtda-ggproto
#' @usage NULL
#' @export
GeomBarcode <- ggproto(
  "GeomBarcode", GeomSegment,
  
  required_aes = c("start|xmin", "end|xmax"),
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # reconcile conflicts: prompt for `xmin,xmax` but use `start,end` internally
    if (is.null(data$start)) {
      data$start <- data$xmin
    } else if (! is.null(data$xmin)) {
      warning("Aesthetic `xmin` was provided, so `start` will be ignored.")
    }
    if (is.null(data$end)) {
      data$end <- data$xmax
    } else if (! is.null(data$xmax)) {
      warning("Aesthetic `xmax` was provided, so `end` will be ignored.")
    }
    
    # introduce numerical x-values in order to allow coordinate transforms
    data$x <- data$start
    data$xend <- data$end
    
    # introduce categorical -> integer y-values
    # in order of `group`, `x`, and `xend`
    # (assumes that `group` is a refinement of dimension)
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
