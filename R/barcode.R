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
NULL

# file.edit("tests/testthat/test-barcode.R")
# file.edit("inst/examples/ex-barcode.R")



#' @rdname barcode
#' @export
stat_barcode <- function(mapping = NULL,
                         data = NULL,
                         geom = "barcode",
                         position = "identity",
                         filtration = "Rips",
                         diameter_max = NULL, radius_max = NULL,
                         dimension_max = 1L,
                         field_order = 2L,
                         engine = NULL,
                         order_by = c("persistence", "start"),
                         decreasing = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    stat = StatBarcode,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      filtration = filtration,
      diameter_max = diameter_max, radius_max = radius_max,
      dimension_max = dimension_max,
      field_order = field_order,
      engine = engine,
      order_by = order_by,
      decreasing = decreasing,
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
  
  required_aes = c("dataset|start", "dataset|end"),
  
  # pre-process of the data set
  setup_data = function(data, params) {
    
    # introduce numerical x-values in order to allow coordinate transforms
    data$x <- data$start
    data$xend <- data$end
    
    # compute vertical position if absent (see `StatPersistence$compute_*()`)
    if (is.null(data$id)) {
      interaction_args <- c(
        # always sort first by `group`
        if (! is.null(data$group)) list(data$group),
        # sort by available properties
        list(data$start, data$end),
        # drop unused levels and use lexicographic order
        list(drop = TRUE, lex.order = TRUE)
      )
      data$y <- do.call(interaction, args = interaction_args)
      # re-distinguish duplicates
      data$y <- order(order(data$y))
    } else {
      # use `id` for vertical position
      data$y <- data$id
    }
    
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
                         stat = "persistence",
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
